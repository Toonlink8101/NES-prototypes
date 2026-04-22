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

	lda #0		;$E8 instead?
	ldx #$10
	ldy #$0
	:
			sta $2007
			dey
		bne:-
		dex
	bne:-
	
	
;write background

;point to nametable 0
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	
;fill top tiles
	;black
	Draw_tile $20, 2+32;*2
	
	;top edge
	Draw_tile $84, 32-4

	
	ldy #3
	@loop_start:
		Draw_tile $20, 4	;black
		
		Draw_tile $82		;white
		
		Draw_tile $20, 26	;black
		
		Draw_tile $82		;white
		
		dey
	bne @loop_start
	
	Draw_tile $20, 4	;black
	Draw_tile $82		;white
	
	Draw_text "         $4011 sound only "

	Draw_tile $82		;white
	
	Draw_tile $20, 4	;black
	Draw_tile $82		;white
	
	Draw_text "  NROM only   No OAM DMA  "

	Draw_tile $82		;white
	
	
	Draw_tile $20, 4	;black
	
	;bottom edge
	Draw_tile $83, 28
	
	;black
	Draw_tile $20, 2+32*2
	
	
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
	
;point to lower portion of nametable 2
	lda #$2A
	sta $2006
	lda #256-64
	sta $2006
	
;fill bottom
	Draw_tile $20, 11	;black
			
	;top edge
	Draw_tile $84, 10
	
	Draw_tile $20, 22	;black
	
	
;	Name

	Draw_tile $82	;white
	Draw_tile $20, 2	;black
	
	Draw_text "Name"
		
	Draw_tile $20, 2	;black
	
	Draw_tile $82	;white
	
	
	Draw_tile $20, 22	;black
	
;	HP

	lda #$82	;white
	sta $2007	

	lda #$20	;blank
	sta $2007
	
	Draw_text "HP: 00"
		
	lda #$20	;blank
	sta $2007
	
	lda #$82	;white
	sta $2007
	
	Draw_tile $20, 22	;black


	;MP

	lda #$82	;white
	sta $2007	

	lda #$20	;blank
	sta $2007
	
	Draw_text "MP: 00"
		
	lda #$20	;blank
	sta $2007
	
	lda #$82	;white
	sta $2007
	
	Draw_tile $20, 22	;black
	
	;bottom edge
	Draw_tile $83, 10	;black
	
	Draw_tile $20, 11	;black
	
	
	Draw_tile $20, 2+32*3	;black

	;reset PPUADDR
	lda #0
	sta $2006
	sta $2006	
	
	;return
	rts