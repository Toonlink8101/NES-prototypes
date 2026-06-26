; Background drawing routine

;Draw_tile
; A ends as tile and count as X. If count is blank, X is unused
.macro Draw_tile tile, count
	.ifblank count
		lda #tile
		sta $2007
	.endif
	.ifnblank count
		lda #tile
		ldx #count
		:
			sta $2007
			dex
		bne:-
	.endif
.endmacro

.macro Draw_text text
	.repeat .strlen(text), I
		lda #.strat(text, I);-$20
		sta $2007
	.endrepeat
.endmacro

.segment "CODE"

Draw_background:

;clear PPU
	lda #$20
	sta $2006
	lda #$00
	sta $2006

	lda #0
	ldx #$10
	ldy #$0
	:
			sta $2007
			dey
		bne:-
		dex
	bne:-
	
; draw map
	lda #$20
	sta $2006
	lda #$00
	sta $2006

	lda zp_PPUctrl_state
	ora #%00000100		;enable +32 tile mode
	sta zp_PPUctrl_state
	sta $2000
	
	lda #<Map
	;sec
	;sbc #32*7
	sta zp_map_addr
	lda #>Map
	;clc
	;adc #4	;start on the 4th page
	sta zp_map_addr+1
	
	bit $2002
	
	ldx #0
draw_map_loop:
		lda #$20
		sta $2006
		stx $2006
		ldy #0
		:
			lda (zp_map_addr), Y
			sta $2007
			iny
			cpy #30
		bne:-
		
		;16bit dec to next line of data
		lda zp_map_addr
		clc
		adc #32
		sta zp_map_addr
		bcc:+
			inc zp_map_addr+1
		:
		
		inx
		cpx #32
	bne draw_map_loop
	
	;reset ptr
	lda #<Map
	;sec
	;sbc #32*7
	sta zp_map_addr
	lda #>Map
	;clc
	;adc #4	;start on the 4th page
	sta zp_map_addr+1
	
	
	lda zp_PPUctrl_state
	and #%11111011		;disable +32 tile mode
	sta zp_PPUctrl_state
	sta $2000
	
	rts
	
	bit $2002
	
	ldx #32
draw_attr_loop:
		lda #$20
		sta $2006
		lda #$C0
		sta $2006

		ldy #30
		lda (zp_map_addr), Y
		sta $2007
		
		ldy #31
		lda (zp_map_addr), Y
		sta $2007
		
		;16bit dec to next line of data
		lda zp_map_addr
		clc
		adc #32
		sta zp_map_addr
		bcc:+
			inc zp_map_addr+1
		:
		
		dex
	bne draw_attr_loop
	
	
	rts
	jmp fill_attributes
	
	
;write background

;point to nametable 0
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	
	
;fill top tiles
	
	ldy #0
	:
		lda Top_textbox_tiles, Y
		sta $2007
		iny
	bne:-
	
	
	Draw_tile $20, 32*2	;black

	
;point to lower portion of nametable 2
	lda #$2A
	sta $2006
	lda #256-64
	sta $2006
	
	
;fill bottom
	ldx #0
	:
		lda Bottom_textbox_tiles, X
		sta $2007
		inx
		cpx #32*5
	bne:-
	
	;if loading exactly 256 tiles and data is aligned, "lda abs,Y" followed by "iny" could be used to save cycles/byte, negating the need for X
	;if loading less than 256 tiles, X would still be needed to count, maybe with "txa" & "axs", but "iny" could still be used
	;However, if the tile count is constant, an "cpy #" could also work, making Y count up to the desired tile count
	
	
	Draw_tile $20, 32*3	;black

fill_attributes:

	;bit $2002
	
	lda zp_PPUctrl_state
	and #%11111011		;disable +32 tile mode
	sta zp_PPUctrl_state
	sta $2000
	
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
	
	
	
	;reset PPUADDR
	lda #0
	sta $2006
	sta $2006	
	
	;return
	rts