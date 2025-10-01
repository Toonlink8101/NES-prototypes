;select extended instruction set
;.P02X

.segment "HEADER"
    .byte "NES"     ;identification string
    .byte $1A
    .byte $02       ;amount of PRG ROM in 16k units
    .byte $01       ;amount of CHR ROM in 8k units
    .byte $00
    .byte $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00

.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "ZEROPAGE"
temp:    .res 1      ;reserves 1 byte of memory

.segment "CODE"
reset:
    sei         ;Disables interupts
    cld         ;Turn off decimal mode

    ldx #%01000000    ;disable IRQ
    stx $4017
    ldx #$00
    stx $4010       ;Disable PCM

    ;Init the stack register
    ldx #$FF
    txs

    ;Clear PPU registers
    ldx #$00
    stx $2000
    stx $2001

    ;Wait for first Vblank
:
    bit $2002
    bpl :-

    ; clear all RAM to 0
	lda #0
	ldx #0
	:
		sta $0000, X
		sta $0100, X
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	; place all sprites offscreen
	lda #$FF
	ldx #0
	:
		sta oam, X
		inx
		inx
		inx
		inx
		bne :-

    ;Wait for second Vblank
:
    bit $2002
    bpl :-

    ;OAM DMA
    lda #>oam
    sta $4014

	;set scroll
    lda #$3f    ;$3f00
    sta $2006
    lda #$00
    sta $2006

    ldx #$00
loadpalettes:
    lda palettedata, x
    sta $2007
    inx
    cpx #$20
    bne loadpalettes


    ldx #$00
loadsprites:
        lda spritedata, x
        sta $0200, x
        inx
        cpx #$10    ;16bytes (4 bytes per sprite, 4 sprites total)
        bne loadsprites

;enable interupts
    cli

    lda #%10010000		; enable NMI, BG pattern table 1
	;lda #%10000000		; BG pattern table 0
    sta $2000

    lda #%00011110          ;show sprites and background
    sta $2001

;init DMC
	lda #0
	sta $4012
	lda #%10001111
	sta $4010
	lda #0
	sta $4011
	sta $4013

; init IRQ audio channels
	lda #1
	sta saw_divider
	lda #%10011001
	sta lfsr


    loop_forever:
        lda #0
		sta temp
		bit temp
		beq loop_forever
		
nmi:
	sei
    lda #>oam    ;OAM DMA
    sta $4014
		
	; trigger DMC
	lda #$10 
	sta $4015 
	sta $4015 
	sta $4015 
	cli
	
	;disable nmi
	;lda #%00010000		; disable NMI, BG pattern table 1
    ;sta $2000
	
	
	rti
	
.segment "ZEROPAGE"
	DMC_output: .res 1 ; init to zero
	DMC_2nd_output: .res 1 ; init to zero, only used once per frame
	preserve_A: .res 1
	preserve_X: .res 1	; might be unneeded
	preserve_Y: .res 1
	;LUT_offset: .res 1

; some of these could be made zeropage for speed
; maybe the volume variable should also hold the waveform
.segment "BSS"
	irq_counter: .res 1
	square1_counter: .res 1	; incremented each IRQ
	square1_divider: .res 1 ; determines wavelength
	square1_volume: .res 1 ; volume is always "4-bit"
	saw_counter: .res 1
	saw_divider: .res 1
	noise_counter: .res 1
	noise_divider: .res 1
	noise_volume: .res 1
	lfsr: .res 1			; init to %10011001 or any non-zero
	pulse_counter: .res 1
	pulse_divider: .res 1
	pulse_volume: .res 1


.segment "RODATA"
; DMC wiggle sample
	.org $C000
	.byte %10101010
	.reloc

.segment "CODE"
IRQ:
	; preserve registers
	sta preserve_A
	; stx preserve_X
	sty preserve_Y

	; set things up, as needed

	; time a write to DMC_output from previous irq
	lda DMC_output
	sta $4011

	; time a screen scroll if queued
	
	
	; acknowledge/reset IRQ
	inc irq_counter
	lda irq_counter
	cmp #63
	bcc:+
		jmp alternate_irq_end
	:
	sei
	lda #$10
	sta $4015
	sta $4015
	sta $4015
	cli

	; Timing here on out is no longer strict

	;;;;
	;Calculate next DMC position
	;;;;

	; init Y to hold new output
	; preserve Y here instead?
	ldy #0

	; Channels currently have set waveforms.
	; Should that change?

	; generate square wave 1
	lda square1_counter
	sec
	sbc #1 
	sta square1_counter
	bne:+
		; reset counter
		lda square1_divider
		lsr
		ora #$80 ; set up overflow
		sta square1_counter
	:
	bvc:+
		; reset counter
		lda square1_divider
		lsr
		adc #0	; handle odd lengths
		sta square1_counter
	:
	; if plus, add volume to output
	bmi:+
		tya
		clc
		adc square1_volume
		tay
	:

	; Generate sawtooth
	; has no volume control. Instead, volume varies by pitch
	dec saw_counter
	bne:+
		lda saw_divider
		sta saw_counter
	:
	lda saw_counter
	and #%00111100
	lsr					;could be alr (unofficial opcode)
	lsr						; carry clear
	adc identity_table, y	; could be "adc Y" macro
	tay

	; generate noise
	bit lfsr						;what if the counter held the
	; if N, add volume to output	;state instead?
	bpl:+
		tya
		clc					; CLC is kinda optional here
		adc noise_volume	; even if C is set, it'll just be noisy
		tay					; which is kinda the point
	:
	dec noise_counter
	bne noise_end
		; reset counter
		lda noise_divider
		sta noise_counter

		; galois lfsr
		lda lfsr
		lsr
		bcc:+
			eor #%10101001
		:
		sta lfsr
noise_end:

	; What if there was a noise that made a thin pulse for
	; each rising/falling edge of the lfsr?
	; pseudo high-pass noise? Probably not too useful tho...


	; generate thin pulse
	dec pulse_counter
	bne:+
		; reset counter
		lda pulse_divider
		sta pulse_counter

		; add volume to output
		tya
		clc
		adc pulse_volume
		tay		; maybe skip to output instead?
				; no need to shift Y around again
	:

	; prepare for output
normal_output:
	tya
	asl
	and #%01111110		; might be unneeded...
	sta DMC_output

	; handle double buffered update

	; restore registers
	lda preserve_A
	; ldx preserve_X
	ldy preserve_Y


	rti
	
alternate_irq_end:
	;disable DMC
	sei
	lda #$00
	sta $4015
	sta $4015
	sta $4015
	cli

	;disable DMC IRQ
	;lda #%00000000
	;sta $4010
	
	lda #0
	sta irq_counter		;reset counter
	
	rti
; end of IRQ


palettedata:
	.byte $00, $0f, $00, $10, 	$00, $0a, $15, $01, 	$00, $29, $28, $27, 	$00, $34, $24, $14 	;background palettes
	.byte $31, $0f, $15, $30, 	$00, $0f, $11, $30, 	$00, $0f, $30, $27, 	$00, $3c, $2c, $1c 	;sprite palettes

spritedata:
;Y, sprite num, attributes, X
;76543210
;||||||||
;||||||++- palette (4 to 7) of sprite
;|||+++--- Unimplemented
;||+------ priority (0: in front of background; 1: behind background)
;|+------- flip sprite horizontally
;+-------- flip sprite vertically

	.byte $40, $00, $00, $40
	.byte $40, $01, $00, $48
	.byte $48, $10, $00, $40
	.byte $48, $11, $00, $48
	
	;sword
	.byte $50, $08, %00000001, $80
	.byte $50, $08, %01000001, $88
	.byte $58, $18, %00000001, $80
	.byte $58, $18, %01000001, $88

.segment "RODATA"
identity_table:
    .byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f
    .byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f
    .byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
    .byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f
    .byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f
    .byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
    .byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f
    .byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f
    .byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
    .byte $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f
    .byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af
    .byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
    .byte $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf
    .byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df
    .byte $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
    .byte $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff


.segment "VECTORS"
    .word nmi
    .word reset
	.word IRQ

.segment "TILES"
    .incbin "rom.chr"