.segment "CODE"

;;;
Vblank:
;;;

	;since this will be interrupted, push preserved registers to stack
	lda preserve_A
	pha
	lda preserve_X
	pha
	lda preserve_Y
	pha


	; since this will be interupted by other IRQs,
	; Iterate software channels
	; trashes A,X,Y
	; returns with output in A
	jsr Iterate_channels
	
	sta DMC_output	
	
;Read controller 1
	;taken from https://www.nesdev.org/wiki/Controller_reading_code#Basic_Example
	;should be 132 cycles
    lda #$01
    sta $4016
    sta zp_buttons
    lsr			; now A is 0
    ; By storing 0 into $4016, the strobe bit is cleared and the reloading stops.
    ; This allows all 8 buttons (newly reloaded) to be read from $4016.
    sta $4016
	:
		lda $4016
		lsr			; bit 0 -> Carry
		rol zp_buttons	; Carry -> bit 0; bit 7 -> Carry
    bcc:-
	
	
	;disable PPU and update state
	lda zp_PPUmask_state
	and #%11100111
	sta zp_PPUmask_state
	
	;lda zp_PPUmask_state
	sta $2001
	
	lda zp_PPUctrl_state
	ora #%00000100		;enable +32 tile mode
	sta zp_PPUctrl_state
	sta $2000

	; clear W
	bit $2002

	lda zp_PPU_update
	beq skip_PPU_update
	bmi tiles_only
		;attribute update
		ldx zp_attr_addr_low
		ldy #8
		:
			lda zp_attr_addr_high
			sta $2006
			stx $2006
		
			lda zp_tile_queue+30-1, Y
			sta $2007
			
			txa
			axs	#256-8	;adds 8 to X
		
			dey
		bne:-
		
tiles_only:
		;point PPU to new column of tiles
		lda zp_column_high
		ldy zp_column_low
		sei
		sta $2006
		sty $2006
		cli
		
		;ldx #30
		ldy #30
		:
			lda zp_tile_queue-1, Y
			sta $2007
			dey
			;dex
		bne:-
		
		
skip_PPU_update:
	
	;reset PPUADDR
	lda #0
	sta $2006
	sta $2006
	
	
	;update OAM
	ldx #0
	stx $2003
	
	;write sprite buffer
	lda #$FF
.repeat 8
	sta $2004
.endrepeat
	
	; update sprites
	ldx #$08	;skip buffer
	:
		;write "count" bytes (4 bytes = 1 sprites)
		count .set 4;*2
		thing .set 0
	.repeat count
		lda $0200+thing, X
		sta $2004
		thing .set thing+1
	.endrepeat
		
		;increment x by count
		txa
		;axs #256- 4
		axs #256- count
		
		cpx #(player_count*4+2) *4	;sprite amount * 4 bytes per sprite
						; +2 is for buffer sprites
	bne:-
	
	;reset OAM ADDR
	;lda #0
	;sta $2003
	
	;reset scroll?
	
	jsr Audio_handler
	
	
	;check if Main finished last frame
	; rti if Main didn't finish?
	
	jmp Main