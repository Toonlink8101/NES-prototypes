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
	;wait for hblank
.repeat 6
	nop
.endrepeat
	
	sty $2001				;4
	
.repeat 4
	nop
.endrepeat
	
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
	
	
	cpx #58
	bne:+
		;reset scroll with full scroll
		
		;update nametable state
		lda #0;%00001000
		sta zp_nametable_state
		
		; get x offset
		lda zp_camera_x
		sta zp_x_offset
		
		
		;get y offset
		lda #0
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
		
		
		;change to scroll routine
		lda #<scroll_IRQ
        sta <zp_irq_addr
        lda #>scroll_IRQ
        sta zp_irq_addr+1
	:
	
	cpx #57
	bne:+
		;enable PPU
		lda zp_PPUmask_state
		ora #%00010000
		sta zp_PPUmask_state
		
		lda #<IRQ
        sta <zp_irq_addr
        lda #>IRQ
        sta zp_irq_addr+1
	:
	
	cpx #56
	bne:+
		;enable PPU
		lda zp_PPUmask_state
		ora #%00011000
		sta zp_PPUmask_state
		
		;lda #<IRQ
        ;sta <zp_irq_addr
        ;lda #>IRQ
        ;sta zp_irq_addr+1
	:
	
	cpx #55
	bne:+
		;change routine to empty
		lda #<empty_IRQ
        sta <zp_irq_addr
        lda #>empty_IRQ
        sta zp_irq_addr+1
	:
	
	cpx #5
	bne:+
		;disable PPU
		lda zp_PPUmask_state
		and #%11100111
		sta zp_PPUmask_state
		
		lda #<IRQ
        sta <zp_irq_addr
        lda #>IRQ
        sta zp_irq_addr+1
	:
	
	cpx #1
	bne:+
		;enable PPU
		lda zp_PPUmask_state
		ora #%00010000
		sta zp_PPUmask_state
		
		lda #<IRQ
        sta <zp_irq_addr
        lda #>IRQ
        sta zp_irq_addr+1
	:
	
	cpx #0
	bne:+
		lda #<empty_IRQ
        sta <zp_irq_addr
        lda #>empty_IRQ
        sta zp_irq_addr+1
	:
	
	
	;skip 4th frame
	cpx #0
	bne:++
		lda #64
		cmp irq_counter
		bne:+
			lda #0
			sta irq_counter
		:
	:

	; Iterate software channels
	; trashes A,X,Y
	; returns with output in A
	jsr Iterate_channels
	
	sta DMC_output
	
	; restore registers
	; 9 cycles
	lda preserve_A
	ldx preserve_X
	ldy preserve_Y
	
end_of_vblank:
	
	;bit $2002
	
	rti



scroll_IRQ:
	; preserve registers
	sta preserve_A
	stx preserve_X
	sty preserve_Y

	; update irq count
	dec irq_counter

	;load slower DMC frequency
	ldx irq_counter
	lda irq_freq_table, X
	sta $4010
	
	;time a PPU update
	lda zp_nametable_state	;3
	sta $2006				;4
	lda zp_y_offset			;3
	sta $2005				;4
	lda zp_x_offset			;3
	ldx zp_XY_offset		;3
	
	;wait for hblank
.repeat 0	;1 cycle left
	nop
.endrepeat
	
	sta $2005				;4
	stx $2006				;4
	
	jmp output_dmc			;3


; uses two $2006 writes to coarsely scroll
; zp_y_offset = 00yyNNYY
; zp_x_offset = YYYXXXXX
coarse_scroll_IRQ:
	; preserve registers
	sta preserve_A
	stx preserve_X
	sty preserve_Y

	; update irq count
	dec irq_counter

	;load slower DMC frequency
	ldx irq_counter
	lda irq_freq_table, X
	sta $4010
	
	;time a PPU update
	ldy zp_PPUmask_state	;3
	lda zp_y_offset		;3
	sta $2006				;4
	lda zp_x_offset		;3
	;wait for hblank
.repeat 2
	nop
.endrepeat
	
	sta $2006				;4
	sty $2001				;4
	
	jmp output_dmc			;3


empty_IRQ:
	; preserve registers
	sta preserve_A
	stx preserve_X
	sty preserve_Y

	; update irq count
	dec irq_counter
	
	;load slower DMC frequency
	ldx irq_counter
	lda irq_freq_table, X
	sta $4010
	
	; for now, wait for timing
	.repeat 14
		nop
	.endrep

	jmp output_dmc
