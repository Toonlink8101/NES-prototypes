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

RESET:
    SEI         ;disables interupts
    CLD         ;turn off decimal mode

    LDX #%10000000    ;disable IRQ
    STX $4017
    LDX #$00
    STX $4010       ;disable PCM

    ;init the stack register
    LDX #$FF
    TXS         ;transfer x to the stack

    ;Clear PPU registers
    LDX #$00
    STX $2000
    STX $2001

    ;WAIT FOR VBLANK
:
    BIT $2002
    BPL :-

    ;CLEARING 2K MEMORY
    TXA
CLEARMEMORY:            ;$0000 - $07FF
    STA $0000,  X
    STA $0100,  X
    STA $0300,  X
    STA $0400,  X
    STA $0500,  X
    STA $0600,  X
    STA $0700,  X
    LDA #$FF
    STA $0200,  X
    LDA #$00
    INX
    CPX #$00
    BNE CLEARMEMORY

    ;WAIT FOR VBLANK
:
    BIT $2002
    BPL :-

    ;SETTING SPRITE RANGE
    LDA #$02
    STA $4014

    LDA #$3F    ;$3F00
    STA $2006
    LDA #$00
    STA $2006

    LDX #$00
LOADPALETTES:
    LDA PALETTEDATA, X
    STA $2007
    INX
    CPX #$20
    BNE LOADPALETTES


    LDX #$00
LOADSPRITES:
        LDA SPRITEDATA, X
        STA $0200, X
        INX
        CPX #$10    ;16bytes (4 bytes per sprite, 4 sprites total)
        BNE LOADSPRITES

;ENABLE INTERUPTS
    CLI

    LDA #%10010000
    STA $2000               ;WHEN VBLANK OCCURS CALL NMI

    LDA #%00011110          ;show sprites and background
    STA $2001


    INFLOOP:
        JMP INFLOOP
NMI:
    LDA #$02    ;LOAD SPRITE RANGE
    STA $4014

PALETTEDATA:
	.byte $00, $0F, $00, $10, 	$00, $0A, $15, $01, 	$00, $29, $28, $27, 	$00, $34, $24, $14 	;background palettes
	.byte $31, $0F, $15, $30, 	$00, $0F, $11, $30, 	$00, $0F, $30, $27, 	$00, $3C, $2C, $1C 	;sprite palettes

SPRITEDATA:
;Y, SPRITE NUM, attributes, X
;76543210
;||||||||
;||||||++- Palette (4 to 7) of sprite
;|||+++--- Unimplemented
;||+------ Priority (0: in front of background; 1: behind background)
;|+------- Flip sprite horizontally
;+-------- Flip sprite vertically

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
    .word NMI
    .word RESET
    ; specialized hardware interrupts
.segment "CHARS"
    .incbin "rom.chr"