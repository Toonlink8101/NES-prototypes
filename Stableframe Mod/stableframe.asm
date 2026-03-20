; PRG ROM

; .include "bitfuncs.inc"

.include "header.asm"

.include "nes.s"

.include "software_channels.asm"

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

.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA

; Game variables
.segment "ZEROPAGE"
	; IRQ trampoline
	zp_irq_jmp: .res 1
	zp_irq_lo: .res 1
	zp_irq_hi: .res 1
	
	; temp vars used in irq routines
	zp_temp_a: .res 1
	zp_temp_y: .res 1
	
	zp_temp_x: .res 1
	
	; low byte of irq align sequence in the table
	zp_irq_align_sequence: .res 1
	
	; joypad read values
	zp_joypad_p0: .res 1
	zp_joypad_p1: .res 1
	
	;software channel variables
	DMC_output: .res 1
	
	
.segment "BSS"
	ram_dmc_sync_display: .res 1
	
	;Constants
	DMC_SAMPLE_ADDR = $C000;FFC0
	
	DEFAULT_PPUMASK = PPUMASK_BACKGROUNDENABLE | PPUMASK_SPRITEENABLE | PPUMASK_BACKGROUNDLEFT8PX | PPUMASK_SPRITELEFT8PX
	DEFAULT_PPUCTRL = PPUCTRL_NAMETABLE2000 | PPUCTRL_SPRITEPATTERN | PPUCTRL_SPRITE16PXMODE | PPUCTRL_BACKGROUNDPATTERN ;| PPUCTRL_VBLANKNMI
	
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
    lda #>oam
    sta $4014
	
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

setup_software_sound:
	lda #<Pulse_chan1
	ldx #>Pulse_chan1
	sta waveforms+0
	stx waveforms+1
	
	channel .set 0*5
	lda #0
	sta channel_vars+channel+divider
	lda #0
	sta channel_vars+channel+counter
	lda #$0
	sta channel_vars+channel+volume
	lda #$AA
	sta channel_vars+channel+lfsr
	lda #0
	sta channel_vars+channel+lfsr_tap
	
	lda #<Pulse_chan2
	ldx #>Pulse_chan2
	sta waveforms+2
	stx waveforms+3
	
	channel .set 1*5
	lda #0
	sta channel_vars+channel+divider
	lda #0
	sta channel_vars+channel+counter
	lda #$0
	sta channel_vars+channel+volume
	lda #$AA
	sta channel_vars+channel+lfsr
	lda #0
	sta channel_vars+channel+lfsr_tap
	
	lda #<LFSR_chan3
	ldx #>LFSR_chan3
	sta waveforms+4
	stx waveforms+5
	
	channel .set 2*5
	lda #4
	sta channel_vars+channel+divider
	lda #1
	sta channel_vars+channel+counter
	lda #$0
	sta channel_vars+channel+volume
	lda #0
	sta channel_vars+channel+lfsr
	lda #0;%11100001
	sta channel_vars+channel+lfsr_tap
	
	;lda #<Sample_chan4
	;ldx #>Sample_chan4
	lda #<LFSR_chan4
	ldx #>LFSR_chan4
	sta waveforms+6
	stx waveforms+7
	
	channel .set 3*5
	;lda #$55
	;sta channel_vars+channel+divider
	;lda #<Snare_sample
	;sta channel_vars+channel+lfsr
	;lda #>Snare_sample
	;sta channel_vars+channel+lfsr_tap
	
	lda #4
	sta channel_vars+channel+divider
	lda #1
	sta channel_vars+channel+counter
	lda #$0
	sta channel_vars+channel+volume
	lda #0
	sta channel_vars+channel+lfsr
	lda #0;%11100001
	sta channel_vars+channel+lfsr_tap

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

.align 64

DMC_wiggle_sample:
	.byte $AA

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