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
	player_y:	.res 1
	player_x:	.res 1
	
	frame_count: .res 1
	pattern_count: .res 1
	detune_active: .res 1

.segment "ZEROPAGE"
; IRQ trampoline
	zp_irq_jmp: .res 1
	zp_irq_addr: .res 2
	
;PPU states and midline scroll vars
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

;other zp vars
	zp_buttons: .res 1
	zp_previous_buttons: .res 1
	
;camera and scrolling
	zp_camera_x: .res 2
	zp_PPU_update: .res 1
	zp_column_high: .res 1
	zp_column_low: .res 1
	zp_map_offset: .res 2
	zp_map_addr: .res 2
	
	zp_tile_queue: .res 30
	
	
;constants
	player_count = 1

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
        cpx #(37 + 4+4*player_count) *4    ;sprite amount * 4 bytes per sprite
        ;cpx #(4+4*player_count) *4    ;sprite amount * 4 bytes per sprite
        bne loadsprites
	
	jsr Draw_background
	
	;reset OAM ADDR
	ldx #0
	stx $2003
	
	;OAM DMA
    lda #>oam
    sta $4014

;enable interupts
    cli


;PPUCTRL bits
;7  bit  0
;---- ----
;VPHB SINN
;|||| ||||
;|||| ||++- Base nametable address
;|||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
;|||| |+--- VRAM address increment per CPU read/write of PPUDATA
;|||| |     (0: add 1, going across; 1: add 32, going down)
;|||| +---- Sprite pattern table address for 8x8 sprites
;||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
;|||+------ Background pattern table address (0: $0000; 1: $1000)
;||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
;|+-------- PPU master/slave select
;|          (0: read backdrop from EXT pins; 1: output color on EXT pins)
;+--------- Vblank NMI enable (0: off, 1: on)

	;lda #%10010000		; enable NMI, BG pattern table 1
	lda #%00110000		; disable NMI, BG pattern table 1
    sta $2000
	sta zp_PPUctrl_state

;PPUMASK bits
;7  bit  0
;---- ----
;BGRs bMmG
;|||| ||||
;|||| |||+- Greyscale (0: normal color, 1: greyscale)
;|||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
;|||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
;|||| +---- 1: Enable background rendering
;|||+------ 1: Enable sprite rendering
;||+------- Emphasize red (green on PAL/Dendy)
;|+-------- Emphasize green (red on PAL/Dendy)
;+--------- Emphasize blue

    ;lda #%00011110          ;show sprites and background, show left edge
    ;lda #%00011000          ;show sprites and background, hide all left edge
    lda #%00011000          ;show sprites and background, hide left edge sprite
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
	
; init player vars
	lda #$50
	sta player_y
	sta player_x
	

; init IRQ audio channels
	lda #<Pulse_chan1
	ldx #>Pulse_chan1
	sta waveforms+0
	stx waveforms+1
	
	lda #<Pulse_chan2
	ldx #>Pulse_chan2
	sta waveforms+2
	stx waveforms+3
	
	lda #<Pulse_chan3
	ldx #>Pulse_chan3
	sta waveforms+4
	stx waveforms+5
	
	lda #<Pulse_chan4
	ldx #>Pulse_chan4
	sta waveforms+6
	stx waveforms+7
	
	;LFSR things 
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
	
	;lda #1
	;sta channel_vars+channel*5+divider
	;lda #1
	;sta channel_vars+channel*5+counter
	;lda #$04
	;sta channel_vars+channel*5+volume
	;lda #$1
	;sta channel_vars+channel*5+lfsr
	;lda #$E8
	;sta channel_vars+channel*5+lfsr_tap


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


.include "IRQs.asm"


.segment "RODATA"

.align 64
	
DMC_wiggle_sample:
	.byte $AA

text_data:
	.byte "These first three lines of text are updated every frame.       "


palettedata:
;background palettes
	.byte $0f, $00, $10, $30, 	$00, $0a, $15, $01, 	$00, $29, $28, $27, 	$00, $34, $24, $14
;sprite palettes
	.byte $0f, $25, $15, $35, 	$00, $02, $22, $3c, 	$00, $1A, $39, $0f, 	$00, $0f, $2d, $10
	
	
.include "SpriteData.asm"

.include "Tables.asm"
	
	
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

.include "Draw_background.asm"
.include "Audio_handler.asm"
.include "Vblank.asm"



;Constants

BUTTON_A      = 1 << 7
BUTTON_B      = 1 << 6
BUTTON_SELECT = 1 << 5
BUTTON_START  = 1 << 4
BUTTON_UP     = 1 << 3
BUTTON_DOWN   = 1 << 2
BUTTON_LEFT   = 1 << 1
BUTTON_RIGHT  = 1 << 0
