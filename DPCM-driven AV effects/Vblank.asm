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
	sta $2000

	; clear W
	bit $2002

	ldx #0

	;point PPU to row 0
	lda #$20
	ldy #3+64+32*0
	sei
	sta $2006
	sty $2006
	cli
	
	ldy #24
	:
		;load immediate twice to simulate loading absolute, X
		lda text_data, X
		sta $2007
		inx
		dey
	bne:-
	
	;point PPU to row 1
	lda #$20
	ldy #3+64+32*1
	sei
	sta $2006
	sty $2006
	cli
	
	ldy #20
	:
		;load immediate twice to simulate loading absolute, X
		lda text_data, X
		sta $2007
		inx
		dey
	bne:-

	;point PPU to row 2
	lda #$20
	ldy #3+64+32*2
	sei
	sta $2006
	sty $2006
	cli
	
	ldy #12
	:
		;load immediate twice to simulate loading absolute, X
		lda text_data, X
		sta $2007
		inx
		dey
	bne:-
	
	
	
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
		count .set 4*2
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
	
	
;read inputs
	lda zp_buttons
	and #%00001010
	lsr
	and zp_buttons
	beq:+
		; Use previous frame's directions
		lda zp_buttons
		eor zp_previous_buttons
		and #%11110000
		eor zp_previous_buttons
		sta zp_buttons
	:
	lda zp_buttons
	and #BUTTON_RIGHT
	beq:+
		inc player_x
	:

	lda zp_buttons
	and #BUTTON_LEFT
	beq:+
		dec player_x
	:
	
	lda zp_buttons
	and #BUTTON_DOWN
	beq:+
		inc player_y
	:
	
	lda zp_buttons
	and #BUTTON_UP
	beq:+
		dec player_y
	:

	;write new y position to OAM mirror
	lda player_y

	count .set 0
.repeat player_count
	sta $0208+4*0+count*16
	sta $0208+4*1+count*16
	adc #8
	sta $0208+4*2+count*16
	sta $0208+4*3+count*16
	adc #7
	count .set count +1
.endrepeat
	
	;write new x position to OAM mirror
	count .set 0
.repeat player_count
	lda player_x
	sta $0208+3+4*0+count*16
	sta $0208+3+4*2+count*16
	adc #8
	sta $0208+3+4*1+count*16
	sta $0208+3+4*3+count*16
	count .set count +1
.endrepeat
	
	
	lda zp_buttons
	sta zp_previous_buttons
	
	
	;pull preserved registers from stack
	pla
	tay
	pla
	tax
	pla
	
	bit $2002
	
	jmp end_of_vblank