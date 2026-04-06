;select extended instruction set
;.P02X

.include "WaveGen.asm"

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

.segment "BSS"
	temp: .res 1
	counter_word: .res 2

.segment "ZEROPAGE"
; IRQ trampoline
	zp_irq_jmp: .res 1
	zp_irq_addr: .res 2
	
;other zp vars
	zp_PPUmask_state: .res 1
	zp_PPUctrl_state: .res 1
	zp_nametable_state: .res 1
	zp_x_offset: .res 1
	zp_XY_offset: .res 1
	zp_y_offset: .res 1
	zp_FX_state: .res 1
	zp_temp:    .res 1      ;reserves 1 byte of memory
	irq_counter: .res 1	
	;NMI_DMC_output: .res 1 ; init to zero, only used once per frame
	

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
    ;lda #>oam
    ;sta $4014

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
        cpx #37 *4    ;sprite amount * 4 bytes per sprite
        bne loadsprites
	
	;OAM DMA
    lda #>oam
    sta $4014
	
;write background

;point to nametable 0
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	
;fill top tiles
	;black
	lda #$90
	ldx #32*2+2
	:
		sta $2007
		dex
	bne:-
	
	;dark grey
	lda #$B7
	ldx #32-4
	:
		sta $2007
		dex
	bne:-
	
	ldx #5
	:
		lda #$90	;black
		sta $2007
		sta $2007
		sta $2007
		sta $2007
		
		lda #$B5	;white
		sta $2007
		
		lda #$B6	;grey
		ldy #32-6
		:
			sta $2007
			dey
		bne:-
		
		lda #$B5	;white
		sta $2007
		dex
	bne:--
	
	;black
	lda #$90
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	
	;dark grey
	lda #$B7
	ldx #32-4
	:
		sta $2007
		dex
	bne:-
	
	;black
	lda #$90
	ldx #2+32*2
	:
		sta $2007
		dex
	bne:-
	
;point to attibute table 0
	lda #$23
	sta $2006
	lda #$D8
	sta $2006
	
;write attibute checker pattern
	lda #%11100100
	
	ldx #5*8
	:
		sta $2007
		
		;lfsr
		ldy #8
		:
			asl
			bcc:+
				eor #%11100001
			:
		dey
		bne:--
		
		dex
	bne:---
	
;point to attibute table 2
	lda #$2B
	sta $2006
	lda #$C0
	sta $2006
	
;write attibute checker pattern
	lda #%11100100
	
	ldx #5*8
	:
		sta $2007
		
		;lfsr
		asl
		bcc:+
			eor #%11100001
		:
		
		dex
	bne:--
	
;point to lower portion of nametable 2
	lda #$2A
	sta $2006
	lda #256-64
	sta $2006
	
;fill black on bottom
	lda #$90
	ldx #0
	:
		sta $2007
		dex
	bne:-
	
;reset PPUADDR
	lda #0
	sta $2006
	sta $2006	

;enable interupts
    cli

	;lda #%10010000		; enable NMI, BG pattern table 1
	lda #%00010000		; disable NMI, BG pattern table 1
    sta $2000
	sta zp_PPUctrl_state

    lda #%00011110          ;show sprites and background
    sta $2001
	sta zp_PPUmask_state

;init DMC
;most handled by dmc_sync
	;lda #<(DMC_wiggle_sample >> 6)
	;sta $4012
	;lda #%10001111
	;sta $4010
	lda #0
	sta $4011
	;sta $4013
	
; init visual effect vars
	;lda #$00
	

; init IRQ audio channels
	lda #<Pulse_chan1
	ldx #>Pulse_chan1
	sta waveforms+0
	stx waveforms+1
	
	channel .set 0*5
	lda #4
	sta channel_vars+channel+divider
	lda #0
	sta channel_vars+channel+counter
	lda #$04
	sta channel_vars+channel+volume
	lda #$AA
	sta channel_vars+channel+lfsr
	lda #4
	sta channel_vars+channel+lfsr_tap
	
	;lda #<Linear_chan2
	;ldx #>Linear_chan2
	lda #<Pulse_chan2
	ldx #>Pulse_chan2
	sta waveforms+2
	stx waveforms+3
	
	channel .set 1*5
	lda #10
	sta channel_vars+channel+divider
	lda #1
	sta channel_vars+channel+counter
	lda #$04
	sta channel_vars+channel+volume
	lda #$AA
	sta channel_vars+channel+lfsr
	lda #10
	sta channel_vars+channel+lfsr_tap
	
	;lda #40
	;sta channel_vars+channel+divider
	;lda #2
	;sta channel_vars+channel+counter
	;lda #$02
	;sta channel_vars+channel+volume
	;lda #4
	;sta channel_vars+channel+lfsr
	;lda #(4 ^$FF)+1	;negates value
	;sta channel_vars+channel+lfsr_tap
	
	;lda #<LFSR_chan3
	;ldx #>LFSR_chan3
	lda #<Pulse_chan3
	ldx #>Pulse_chan3
	sta waveforms+4
	stx waveforms+5
	
	channel .set 2*5
	lda #6
	sta channel_vars+channel+divider
	lda #2
	sta channel_vars+channel+counter
	lda #$04
	sta channel_vars+channel+volume
	lda #$AA
	sta channel_vars+channel+lfsr
	lda #6
	sta channel_vars+channel+lfsr_tap
	
	;lda #4
	;sta channel_vars+channel+divider
	;lda #1
	;sta channel_vars+channel+counter
	;lda #$0
	;sta channel_vars+channel+volume
	;lda #1
	;sta channel_vars+channel+lfsr
	;lda #%11100001
	;sta channel_vars+channel+lfsr_tap
	
	;lda #<Sample_chan4
	;ldx #>Sample_chan4
	lda #<Pulse_chan4
	ldx #>Pulse_chan4
	sta waveforms+6
	stx waveforms+7
	
	channel .set 3*5
	lda #15
	sta channel_vars+channel+divider
	lda #3
	sta channel_vars+channel+counter
	lda #$04
	sta channel_vars+channel+volume
	lda #$AA
	sta channel_vars+channel+lfsr
	lda #15
	sta channel_vars+channel+lfsr_tap
	
	;lda #$55
	;sta channel_vars+channel+divider
	;lda #<Snare_sample
	;sta channel_vars+channel+lfsr
	;lda #>Snare_sample
	;sta channel_vars+channel+lfsr_tap

;sync DMC with Vblank
dmc_sync:
	jsr initial_dmc_sync
	
; trigger DMC
; handled elsewhere
	;sei
	;lda #$10 
	;sta $4015 
	;sta $4015 
	;sta $4015 
	;cli

    loop_forever:
		;ldx #0
		;stx zp_temp
		;bit zp_temp
		;inc temp, X
		;bne loop_forever
		
		inc counter_word
		bne:+
			inc counter_word+1
		:
		
		jmp loop_forever
	
	
;dummy nmi	
nmi:
	rti
	
.segment "ZEROPAGE"
	DMC_output: .res 1 ; init to zero
	preserve_A: .res 1
	preserve_X: .res 1
	preserve_Y: .res 1
	;LUT_offset: .res 1

; some of these could be made zeropage for speed
; maybe the volume variable should also hold the waveform
.segment "BSS"
	irq_active: .res 1


.segment "CODE"
IRQ:
	; preserve registers
	sta preserve_A
	stx preserve_X
	sty preserve_Y

	; update irq count
	dec irq_counter
	;clc
	
	; flag vblank
	;bne:+
	;	sec
	;:

	;load slower DMC frequency
	ldx irq_counter
	lda irq_freq_table, X
	sta $4010
	
	;keep freq in Y for later
	;tay
	
	;time a PPU update
	ldy zp_PPUmask_state	;3
	lda zp_x_offset			;3
	;wait for hblank
.repeat 5
	nop
.endrepeat
	
	sta $2005				;4
	bit $2002				;4
	sty $2001				;4
	;bit $2002
	
	; for now, wait for timing
	; 12*2 + 3*4 = 36 cycles
	;.repeat 12
	;	nop
	;.endrep
	;bit $2002
	;bit $2002
	;bit $2002
	
output_dmc:
	
	; time a write to DMC_output from previous irq
	lda DMC_output
	sta $4011
	
	;store fast DMC frequency
	lda #%10001111
	sta $4010
	
	;Acknowledge/reset IRQ
	lda #$10
	sei
	sta $4015
	sta $4015
	sta $4015
	cli

	; Timing here on out is no longer strict
timing_over:

	; preserve Y
	;sty preserve_Y
	
	; get irq position
	lda irq_counter
	and #%00111111		;modulous 64
	tax

	; check for vblank
	;lda irq_freq_table, X
	;cmp #$88
	cpx #63
	bne:+
		jmp Vblank
	:
	
	; before midscreen
	cpx #39
	bne:+
		;clear color emphasis and grayscale
		lda zp_PPUmask_state
		and #%00011110
		sta zp_PPUmask_state
	:
	
	; start of midscreen
	cpx #38
	bne:+
		;update nametable state
		lda #%00001000
		sta zp_nametable_state
		
		;change scroll routine
		lda #<scroll_IRQ
        sta <zp_irq_addr
        lda #>scroll_IRQ
        sta zp_irq_addr+1
	:
	
	;test if irq is midscreen: between 39 and 16 (inclusive)
	txa
	;clc	;carry already clear
	adc #$ff-39		;$ff-max
	adc #39-16+1	;max-min+1
	bcc:+
		;turn grayscale off
		;lda zp_PPUmask_state
		;and #%11111110
		;sta zp_PPUmask_state
		
		;increment FX state
		ldy zp_FX_state
		dey
		sty zp_FX_state
		
		; get x offset
		lda sine_lookup, Y
		sta zp_x_offset
		
		;calc y pos
		txa
		eor #63
		;clc		;carry clear
		;adc #256-4	;subtract 4
		;adc #256-8	;subtract 8
		asl
		asl
		asl
		asl
		
		;get y offset
		and #%01110000
		;clc		;carry clear
		adc sine_lookup, Y
		sta zp_y_offset

			
		;calc course XY offset
		;lda zp_y_offset
		and #%11111000
		asl
		asl
		tay
		lda zp_x_offset
		lsr
		lsr
		lsr
		ora identity_table, Y
		
		sta zp_XY_offset
	:
	; end of midscreen
	cpx #15
	bne:+
		;update nametable state
		lda #%00001000
		sta zp_nametable_state
		
		;turn grayscale on
		;lda zp_PPUmask_state
		;ora #%00000001
		;sta zp_PPUmask_state
		
		;reset x offset and nametable
		lda #0
		sta zp_x_offset
		lda #%00001000
		sta zp_nametable_state
		
		;calc y pos
		txa
		eor #63
		;clc		;carry clear
		adc #$FB	;subtract 5
		asl
		asl
		sta zp_y_offset

			
		;calc course XY offset
		;lda zp_y_offset
		and #%11111000
		asl
		asl
		tay
		lda zp_x_offset
		lsr
		lsr
		lsr
		ora identity_table, Y
		
		sta zp_XY_offset
	:
	cpx #14
	bne:+
		;color emphasis and grayscale
		lda zp_PPUmask_state
		ora #%00000000
		sta zp_PPUmask_state
	
		;change scroll routine
		lda #<IRQ
        sta <zp_irq_addr
        lda #>IRQ
        sta zp_irq_addr+1
	:

	; Iterate software channels
	; trashes A,X,Y
	; returns with output in A
	jsr Iterate_channels
	
	sta DMC_output
	
end_of_vblank:
	
	; restore registers
	; 9 cycles
	lda preserve_A
	ldx preserve_X
	ldy preserve_Y
	
	bit $2002
	
	rti



scroll_IRQ:
	; preserve registers
	sta preserve_A
	stx preserve_X
	sty preserve_Y

	; update irq count
	dec irq_counter
	;clc
	
	; flag vblank
	;bne:+
	;	sec
	;:

	;load slower DMC frequency
	ldx irq_counter
	lda irq_freq_table, X
	sta $4010
	
	;keep freq in Y for later
	;tay
	
	;time a PPU update
	
	lda zp_nametable_state	;3
	sta $2006				;4
	lda zp_y_offset			;3
	sta $2005				;4
	lda zp_x_offset			;3
	ldx zp_XY_offset		;3
	;wait for hblank
.repeat 2
	nop
.endrepeat
	
	sta $2005				;4
	stx $2006				;4
	
	; for now, wait for timing
	; 12*2 + 3*4 = 36 cycles
	;.repeat 12
	;	nop
	;.endrep
	;bit $2002
	;bit $2002
	;bit $2002
	
;output_dmc:
	
	; time a write to DMC_output from previous irq
	lda DMC_output
	sta $4011
	
	;store fast DMC frequency
	lda #%10001111
	sta $4010
	
	;Acknowledge/reset IRQ
	lda #$10
	sei
	sta $4015
	sta $4015
	sta $4015
	cli

	jmp timing_over

; end of IRQs

;;;
Vblank:
;;;
	; since this will be interupted by other IRQs,
	; Iterate software channels
	; trashes A,X,Y
	; returns with output in A
	jsr Iterate_channels
	
	sta DMC_output	
	
	
	;update PPU
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	
	;lda zp_PPUmask_state
	;sta $2001
	
	lda zp_PPUctrl_state
	sta $2000
	
	;AAAAAAAAAAA
	;lda #$9B
	;sta temp
	;ldx #60
	;:
	;	lda temp
	;	sta $2007
	;	dex
	;bne:-
	
	
	;reset PPUADDR
	lda #0
	sta $2006
	sta $2006
	
	;reset scroll?
	
	jmp end_of_vblank


palettedata:
;background palettes
	.byte $0f, $00, $10, $30, 	$00, $0a, $15, $01, 	$00, $29, $28, $27, 	$00, $34, $24, $14
;sprite palettes
	.byte $0f, $25, $15, $35, 	$00, $02, $22, $3c, 	$00, $1A, $39, $0f, 	$00, $0f, $2d, $10
	
spritedata:
;Y, sprite num, attributes, X
;76543210
;||||||||
;||||||++- palette (4 to 7) of sprite
;|||+++--- Unimplemented
;||+------ priority (0: in front of background; 1: behind background)
;|+------- flip sprite horizontally
;+-------- flip sprite vertically

;right arm
	;red
	.byte $80, $60, $00, $60
	.byte $80, $61, $00, $68
	.byte $88, $70, $00, $60
	.byte $88, $71, $00, $68
	;grey 
	.byte $89, $55, $03, $6B
	
;left arm
	;blue
	.byte $81, $30, $01, $85
	.byte $80, $31, $01, $8D
	.byte $88, $40, $01, $87
	.byte $88, $41, $01, $8F
	.byte $90, $50, $01, $84
	.byte $90, $51, $01, $8C
	;grey
	.byte $88, $56, $03, $7F
	
;body
	;legs
	.byte $90, $66, $03, $79
	.byte $97, $76, $03, $79
	.byte $97, $75, $03, $71
	
	;face
	.byte $76, $35, $03, $71
	.byte $77, $36, $03, $79
	.byte $7E, $45, $03, $71
	.byte $7F, $46, $03, $79

;"armor"
	;chestplate
	.byte $86, $53, $02, $77	;middle plate
	.byte $86, $52, $02, $6F	;right shoulder	
	.byte $8E, $62, $02, $6F
	.byte $8E, $63, $02, $77
	.byte $96, $72, $02, $6F
	.byte $96, $73, $02, $77
	.byte $81, $54, $02, $7F	;left shoulder
	
	
	;helmet
	.byte $6F, $32, $02, $70
	.byte $6F, $33, $02, $78
	.byte $71, $34, $02, $80
	.byte $77, $42, $02, $70
	.byte $77, $43, $02, $78
	.byte $79, $44, $02, $80
	
;highlights
	.byte $7B, $37, $01, $72
	.byte $78, $47, $81, $7B
	.byte $7C, $48, $80, $78
	
;bottom of red
	.byte $90, $80, $00, $60
	.byte $90, $81, $00, $68

.segment "RODATA"

.align 256

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
	
irq_freq_table:

;smoother audio, but no OAM DMA & less visual IRQs
;$88 signals vblank
	;frame 1 (2 cycles longer)
	.byte $8d
	
	.byte $8D,$8d
	.byte $8D,$8d,$8d, $8D,$8d,$8d,   $8D,$8d
	
	.byte $8D,$8E,$8E, $8D,$8E,$8E,   $8D,$8E
	.byte $8D,$8E,$8E, $8D,$8E,$8E,   $8D,$8E
	.byte $8D,$8E
	.byte $8D,$8E,$8E, $8D,$8e,$8e,   $8D,$8e
	.byte $8D,$8e,$8e, $8D,$8e,$8d,   $8D,$8d
	
	.byte $8D,$8d
	.byte $8D,$8d,$8d, $8D,$8d,$8d,   $8d,$8d
	
	.byte $8a,$8d,$8a, $8a,$8a,$8a,  $8a,$8e
	
	.byte $88
	
	
.repeat 3
	;frame 2-4
	.byte $8d
	
	.byte $8D,$8d
	.byte $8D,$8d,$8d, $8D,$8d,$8d,   $8D,$8d
	
	.byte $8D,$8E,$8E, $8D,$8E,$8E,   $8D,$8E
	.byte $8D,$8E,$8E, $8D,$8E,$8E,   $8D,$8E
	.byte $8D,$8E
	.byte $8D,$8E,$8E, $8D,$8e,$8e,   $8D,$8e
	.byte $8D,$8e,$8e, $8D,$8e,$8d,   $8D,$8d
	
	.byte $8D,$8d
	.byte $8D,$8d,$8d, $8D,$8d,$8d,   $8d,$8d
	
	.byte $8a,$8d,$8a, $8a,$8a,$8a,  $8c,$8c
	
	.byte $88

.endrepeat
	
	; DPCM playback rates & their timing in CPU cycles
	;Rate  		  $0     $1     $2     $3     $4     $5     $6     $7     $8     $9     $A     $B     $C     $D     $E     $F
	;NTSC 		 428,   380,   340,   320,   286,   254,   226,   214,   190,   160,   142,   128,   106,    84,    72,    54
	;Difference	   ^-48-V ^-40-V ^-20-V ^-34-V ^-32-V ^-28-V ^-12-V ^-24-V ^-30-V ^-18-V ^-14-V ^-22-V ^-22-V ^-12-V ^-18-V
	
.align 64
	
DMC_wiggle_sample:
	.byte $AA
	

sine_lookup:
	.byte 4,4,5,6,7,7,7,7,6,6,5,4,3,2,1,1,0,0,0,0,1,2,2,3,4,5,6,6,7,7,7,7,6,5,4,3,2,2,1,0,0,0,0,1,1,2,3,4,5,6,6,7,7,7,7,6,5,5,4,3,2,1,0,0,0,0,1,1,2,3,4,5,5,6,7,7,7,7,6,6,5,4,3,2,1,1,0,0,0,0,1,2,3,3,4,5,6,7,7,7,7,6,6,5,4,3,2,1,1,0,0,0,0,1,1,2,3,4,5,6,6,7,7,7,7,6,5,4,3,3,2,1,0,0,0,0,1,1,2,3,4,5,6,6,7,7,7,7,6,5,5,4,3,2,1,1,0,0,0,0,1,2,3,4,5,5,6,7,7,7,7,6,6,5,4,3,2,1,1,0,0,0,0,1,2,2,3,4,5,6,7,7,7,7,6,6,5,4,3,2,2,1,0,0,0,0,1,1,2,3,4,5,6,6,7,7,7,7,6,5,4,4,3,2,1,0,0,0,0,1,1,2,3,4,5,6,6,7,7,7,7,6,6,5,4,3,2,1,1,0,0,0,0,1,2,3
	

;LUT_offset: .res 1
.segment "VECTORS"
    ;nmi
	.word nmi
	;reset
    .word reset
	;irq
	.word zp_irq_jmp

.segment "TILES"
    .incbin "rom.chr"
	
.segment "CODE"
;various includes
.include "sync_vbl_long.asm"
.include "dmc_sync.asm"