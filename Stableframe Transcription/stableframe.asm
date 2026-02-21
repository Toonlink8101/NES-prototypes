; PRG ROM

; .include "bitfuncs.inc"

.include "header.asm"

.include "nes.s"

; Macros

    ;MACEXP_DFT  nomacro, noif

;.macro WORD reg
;	.ifnblank reg
;        .byt    <(reg), >(reg)
;	.endif
;.endmacro


.macro SETMEM_DMCADDRESS TARGETADDR
        .if (TARGETADDR & 63) <> 0
            .error "Address must be divisible by 64"
        .endif
        .if !(TARGETADDR >= $c000)
            .error "Address must be >= $c000"
        .endif
        lda #(TARGETADDR - $c000) / 64
        sta DMCADDR
    .endmacro


.macro JUMP_SLIDE CYCLES
        .if (CYCLES & 1) = 1
            .error "Need a cycle count divisible by 2."
        .endif
        ; jump slide
        .repeat (CYCLES / 2) - 2
            cmp #$C9
        .endrepeat
        bit $EA
    .endmacro


    ; SLEEP macro calls into sleep_routine method (with a nop slide before it to adjust timing)
.macro SLEEP ARGCYCLES
        .if ((ARGCYCLES & 1) <> 0)
            .error "Cycle must be even"
        .endif
        .if ARGCYCLES < 10
            .repeat ARGCYCLES / 2
                nop
            .endrepeat
        .elseif ARGCYCLES > 138
            .error "Cycles count cannot exceed 138"
        .else
            jsr sleep_routine - ((ARGCYCLES - 10) / 2)
        .endif
    .endmacro


    ; Simplify writing to nametable
.macro PRINT_STRING XPOS, YPOS, STRING
        lda #(((YPOS * 32) + XPOS) / 256 + $20)
        sta PPUADDR
        lda #(((YPOS * 32) + XPOS) & 255)
        sta PPUADDR

		.repeat .strlen(STRING), I
			lda #.strat(STRING, I)
			sta PPUDATA
		.endrepeat
    .endmacro

; Game variables
.segment "ZEROPAGE"
	; IRQ trampoline
	zp_irq_jmp: .res 1
	zp_irq_lo: .res 1
	zp_irq_hi: .res 1
	
	; temp vars used in irq routines
	zp_temp_a: .res 1
	zp_temp_y: .res 1
	
	; low byte of irq align sequence in the table
	zp_irq_align_sequence: .res 1
	
	; joypad read values
	zp_joypad_p0: .res 1
	zp_joypad_p1: .res 1
	
.segment "BSS"
	ram_dmc_sync_display: .res 1
	
	;Constants
	DMC_SAMPLE_ADDR = $FFC0
	
	DEFAULT_PPUMASK = PPUMASK_BACKGROUNDENABLE | PPUMASK_SPRITEENABLE | PPUMASK_BACKGROUNDLEFT8PX | PPUMASK_SPRITELEFT8PX
	DEFAULT_PPUCTRL = PPUCTRL_NAMETABLE2000 | PPUCTRL_SPRITEPATTERN | PPUCTRL_SPRITE16PXMODE | PPUCTRL_BACKGROUNDPATTERN
	
;---prg start---

.segment "CODE"
reset:
	sei
	lda #0
	sta PPUCTRL
	sta PPUMASK
	sta APUSTATUS
	lda #$40
	sta JOYPADP1READ
	cld
	ldx #$FF
	txs
	
	;wait for 2 vblanks
	ldx #3
:
	bit PPUSTATUS
	bpl:-
	dex
	bne:-
	
clear_internal_ram:
	lda #0
	ldx #0
:
	sta $0000, x
	sta $0100, x
	sta $0200, x
	sta $0300, x
	sta $0400, x
	sta $0500, x
	sta $0600, x
	sta $0700, x
	inx
	bne:-
	
setup_oam:
	;clear oam
	lda #$02
	sta OAMDMA
	
setup_ppu:
	;clear latch
	lda PPUSTATUS
	
	;load palette table into vram
	lda #>(VRAM_PALETTETABLE)
	sta PPUADDR
	lda #<(VRAM_PALETTETABLE)
	sta PPUADDR
:
	lda table_palette, X
	sta PPUDATA
	inx
	cpx #$20
	bcc:-

setup_nametable:
	;update all of nametable $2000 via loop
	lda #>(VRAM_NAMETABLE0)
	sta PPUADDR
	lda #<(VRAM_NAMETABLE0)
	sta PPUADDR
	lda #$04
	ldx #(960 / 8)
:
	sta PPUADDR
	sta PPUADDR
	sta PPUADDR
	sta PPUADDR
	sta PPUADDR
	sta PPUADDR
	sta PPUADDR
	sta PPUADDR
	dex
	bne:-
	
	;set uppper icon
	lda #>(VRAM_NAMETABLE0 + (32*4))
	sta PPUADDR
	lda #<(VRAM_NAMETABLE0 + (32*4))
	sta PPUADDR
	lda #9
	sta PPUADDR
	lda #10
	sta PPUADDR
	lda #11
	sta PPUADDR
	
	lda #>(VRAM_NAMETABLE0 + (32*5))
	sta PPUADDR
	lda #<(VRAM_NAMETABLE0 + (32*5))
	sta PPUADDR
	lda #9+16
	sta PPUADDR
	lda #10+16
	sta PPUADDR
	lda #11+16
	sta PPUADDR
	
	lda #>(VRAM_NAMETABLE0 + (32*6))
	sta PPUADDR
	lda #<(VRAM_NAMETABLE0 + (32*6))
	sta PPUADDR
	lda #9+32
	sta PPUADDR
	lda #10+32
	sta PPUADDR
	lda #11+32
	sta PPUADDR
	
	lda #>(VRAM_NAMETABLE0 + (32*55))
	sta PPUADDR
	lda #<(VRAM_NAMETABLE0 + (32*55))
	sta PPUADDR
	lda #5
	sta PPUADDR
	lda #6
	sta PPUADDR
	lda #7
	sta PPUADDR
	
	lda #>(VRAM_NAMETABLE0 + (32*56))
	sta PPUADDR
	lda #<(VRAM_NAMETABLE0 + (32*56))
	sta PPUADDR
	lda #5+16
	sta PPUADDR
	lda #6+16
	sta PPUADDR
	lda #7+16
	sta PPUADDR
	
	lda #>(VRAM_NAMETABLE0 + (32*57))
	sta PPUADDR
	lda #<(VRAM_NAMETABLE0 + (32*57))
	sta PPUADDR
	lda #5+32
	sta PPUADDR
	lda #6+32
	sta PPUADDR
	lda #7+32
	sta PPUADDR
	
	
	;update the left of the nametable with triangles
	; do so in 32-tile increment mode
	lda #DEFAULT_PPUCTRL | PPUCTRL_INCREMENTMODE
	sta PPUCTRL
	lda #>(VRAM_NAMETABLE0 + $e1)
	sta PPUADDR
	lda #<(VRAM_NAMETABLE0 + $e1)
	sta PPUADDR
	lda #$08
	ldx #16
:
	sta PPUDATA
	dex
	bne:-
	lda #>(VRAM_NAMETABLE0 + $e8)
	sta PPUADDR
	lda #<(VRAM_NAMETABLE0 + $e8)
	sta PPUADDR
	lda #$08
	ldx #16
:
	sta PPUDATA
	dex
	bne:-
	lda #DEFAULT_PPUCTRL
	sta PPUCTRL

pre_sync:
	;sync with vblank before writing to PPU
:
	bit PPUSTATUS
	bpl:-
	
	;set on-screen instructions
	PRINT_STRING 4, 5, "PRESS A TO SYNC  "
	PRINT_STRING 4, 24, "            "
	
	;clear PPUADDR and PPUSCROLL
	lda #0
	sta PPUADDR
	sta PPUSCROLL
	sta PPUSCROLL
	
	;reset PPUCTRL
	lda #DEFAULT_PPUCTRL
	sta PPUCTRL
	;setup PPUMASK
	lda #DEFAULT_PPUMASK
	sta PPUMASK
	
	; wait for A button
button_A_wait:
	jsr read_joypad
	lda zp_joypad_p0
	and #BUTTON_A
	beq button_A_wait
	
	;begin DMC timer sync. Once complete, we enter the main loop
dmc_sync:
	jsr initial_dmc_sync
	
	;by the time this is called, the irq chain will be running, so you can safely perform game logic
main_loop:
	; looping cycle of dummy opcodes on main thread, with 7 cycle instructions to show IRQ jitter
	ldx #0
loop_end:
	inc $0100, x
	bne loop_end
	inc $0101, x
	bne loop_end
	jmp loop_end
	
	
;--- "vblank" routine---

;this routine is not called by NMI, but rather the irq routine that is synced to vblank (start of scanline 241)
vblank_from_irq:
	;clear PPU latch
	lda PPUSTATUS
	;update on-screen instructions
	PRINT_STRING 4, 5, "PRESS B TO DESYNC"
	PRINT_STRING 4, 24, "DMA SYNC $"
	lda ram_dmc_sync_display
	ror
	ror
	ror
	ror
	clc
	and #%1111
	adc #'0'
	sta PPUDATA
	lda ram_dmc_sync_display
	and #%1111
	adc #'0'
	sta PPUDATA
	
	;reset PPUADDR after updating PPU
	lda #0
	sta PPUADDR
	sta PPUADDR
	
	;read joypad and respond
	jsr read_joypad
	lda zp_joypad_p0
	and #BUTTON_LEFT
	bne button_left
	lda zp_joypad_p0
	and #BUTTON_RIGHT
	bne button_right
	lda zp_joypad_p0
	and #BUTTON_B
	bne button_b_reset
	rts
	
	;pressing left/right adjusts the "align" frame, for debugging
button_left:
	lda #<(irq_routines_table_align_0)
	sta zp_irq_align_sequence
	rts
	
button_right:
	lda #<(irq_routines_table_align_1)
	sta zp_irq_align_sequence
	rts
	
	;Pressing B resets the demo
button_b_reset:
	; The method is called during an interrupt, and since we don't know where main execution is, we need to reset the stack
	ldx #$FF
	txs
	
	;clear stack
	lda #0
	ldx #0
:
	sta $0100, x
	inx
	bne:-
	
	;Add reset vector into the stack 
	lda #>(pre_sync)
	pha
	lda #<(pre_sync)
	pha
	; push state to enable interrupts
	lda #$04
	pha
	; acknowledge and disable DMC IRQ
	lda #0
	sta PPUSTATUS
	; return from irq, soft reset
	rti
	
.segment "RODATA"

; ---ppu palette table---

table_palette:
	;background
	.byte $21, $21, $0d, $8c
	.byte $21, $21, $14, $38
	.byte $21, $21, $14, $38 
	.byte $21, $21, $14, $0f
	
	;sprites
	.byte $21, $21, $11, $31
	.byte $21, $21, $11, $31
	.byte $21, $21, $11, $31
	.byte $21, $21, $11, $31
	
.org DMC_SAMPLE_ADDR

	.byte %10101010

.reloc

; other includes

.include "joypad.asm"
.include "sync_vbl_long.asm"
.include "dmc_sync.asm"
.include "irq_routines.asm"
.include "irq_routines_table.asm"

.segment "CODE"
; dummy nmi, never used
nmi:
	rti

.segment "VECTORS"
	; nmi
	.byte <(nmi), >(nmi)
	; reset
	.byte <(reset), >(reset)
	; irq trampoline
	.byte <(zp_irq_jmp), >(zp_irq_jmp)
	
.segment "TILES"
	.incbin "HelloWorld.chr"