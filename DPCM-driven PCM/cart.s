.segment "HEADER"
    .byte "NES"     ;identification string
    .byte $1A
    .byte $02       ;amount of PRG ROM in 16k units
    .byte $01       ;amount of CHR ROM in 8k units
    .byte $00
    .byte $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00
.segment "ZEROPAGE"
VAR:    .RES 1      ;reserves 1 byte of memory
.segment "STARTUP"

reset:
    sei         ;Disables interupts
    sld         ;Turn off decimal mode

    ldx #%10000000    ;disable IRQ
    stx $4017
    ldx #$00
    stx $4010       ;Disable PCM

    ;Init the stack register
    ldx #$FF
    txs         ;Transfer x to the stack

    ;Clear PPU registers
    ldx #$00
    stx $2000
    stx $2001

    ;Wait for Vblank
:
    bit $2002
    bpl :-

    ;clearing 2k memory
    txa
clearmemory:            ;$0000 - $07ff
    sta $0000,  x
    sta $0100,  x
    sta $0300,  x
    sta $0400,  x
    sta $0500,  x
    sta $0600,  x
    sta $0700,  x
    lda #$ff
    sta $0200,  x
    lda #$00
    inx
    cpx #$00
    bne clearmemory

    ;Wait for Vblank
:
    bit $2002
    bpl :-

    ;setting sprite range
    lda #$02
    sta $4014

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

;enable interUpts
    cli

    lda #%10010000
    sta $2000               ;When Vblank occurs call nmi

    lda #%00011110          ;show sprites and background
    sta $2001


    loop_forever:
        jmp loop_forever
nmi:
    lda #$02    ;load sprite range
    sta $4014
	rti

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


.segment "VECTORS"
    .word nmi
    .word reset
    ; specialized hardware interrupts
.segment "CHARS"
    .incbin "rom.chr"