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