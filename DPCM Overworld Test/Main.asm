Main:
;scroll handling
	;move camera
	lda zp_camera_x
	clc
	adc zp_camera_speed
	sta zp_camera_x
	bcc:+
		inc zp_camera_x+1
	:
	bvc:+
		lda zp_camera_x
		cmp #$FF
		bne:+
			dec zp_camera_x+1
	:
	
	
	;bne:+
	;	inc zp_camera_x+1
	;:
	
	
	;check if PPU draw needed
	lda zp_camera_speed
	bne:+
		lda #0
		sta zp_PPU_update
		
		jmp skip_draw_get
	:
	
	ldx #0
	lda zp_camera_x
	and #%00000111
	bne:+
		ldx #$FF
	:
	stx zp_PPU_update
	
	cpx #$FF
	beq:+
		jmp skip_draw_get
	:
	
	bit zp_camera_speed
	bpl:+
		jmp draw_left
	:
	;get PPU draw address
		lda zp_camera_x
		lsr
		lsr
		lsr
		sta zp_column_low
		;high byte is either $20 or $28
		lda #$20
		sta zp_column_high
		
		;get offset for reading map
		lda zp_camera_x
		sta zp_map_offset
		lda zp_camera_x+1
		sta zp_map_offset+1
		
		;16bit asl twice
		asl zp_map_offset
		rol zp_map_offset+1
		asl zp_map_offset
		lda zp_map_offset+1
		rol
		and #%00000111			;limit to 8 pages
		sta zp_map_offset+1
		
		clc
		lda #<Map
		adc zp_map_offset
		sta zp_map_addr
		lda #>Map
		adc zp_map_offset+1
		sta zp_map_addr+1
		
		;ldy zp_map_offset
		;tay
		ldy #0
		ldx #30
		:
			lda (zp_map_addr), Y
			sta zp_tile_queue-1, X
			iny
			dex
		bne:-
		
		;skip attributes for testing
		;jmp skip_draw_get
		
		;check for attribute update
		lda zp_camera_x
		and #%00011111
		cmp #0+8;*2;*3
		;beq new_attr_right_hard
		bcc:+
			cmp #16
			bcc new_attr_right_hard
		:
		cmp #16+8;*2;*3
		;beq new_attr_right_easy
		bcc:+
			cmp #32
			bcc new_attr_right_easy
			
		:
		jmp skip_draw_get
		
new_attr_right_easy:
		;flag update
		lda zp_PPU_update
		and #%01111111
		sta zp_PPU_update
		
		;get high byte from nametable
		lda zp_column_high
		clc
		adc #3
		sta zp_attr_addr_high
		
		lda zp_camera_x
		rol
		rol
		rol
		rol
		anc #%00000111	;clc
		adc #$C0
		sta zp_attr_addr_low	;= attribute base + (scroll / 32)
		
		
		;16bit dec zp_map_addr by 32*3
		lda zp_map_addr
		sec
		sbc #32*3
		sta zp_map_addr
		bcs:+
			dec zp_map_addr+1
		:
		
		;get attribute buffer offset
		lda zp_camera_x
		alr #%11111000		;and #i, then lsr
		lsr					;carry clear
		adc #256-6
		sta zp_attr_buf_offset
		
		ldx #8
		:
			;update tile queue and buffer
			ldy #30						;queue update
			lda (zp_map_addr), Y
			sta zp_tile_queue+30-1, X
			ldy zp_attr_buf_offset		;buffer update
			sta attribute_buffer, Y
			dex							;step
			ldy #31						;queue update
			lda (zp_map_addr), Y
			sta zp_tile_queue+30-1, X
			ldy zp_attr_buf_offset		;buffer update
			sta attribute_buffer+1, Y
			
			;16bit inc zp_map_addr by 32
			lda zp_map_addr
			clc
			adc #32
			sta zp_map_addr
			bcc:+
				inc zp_map_addr+1
			:
			
			;inc attribute buffer offset
			lda zp_attr_buf_offset
			clc
			adc #2
			;and #%00111111
			sta zp_attr_buf_offset
			
			dex							;step
		bne:--
		
		jmp skip_draw_get
		
		
new_attr_right_hard:
		;flag update
		lda zp_PPU_update
		and #%01111111
		sta zp_PPU_update
		
		;get high byte from nametable
		lda zp_column_high
		clc
		adc #3
		sta zp_attr_addr_high
		
		lda zp_camera_x
		rol
		rol
		rol
		rol
		anc #%00000111	;clc
		adc #$C0
		sta zp_attr_addr_low	;= attribute base + (scroll / 32)
		
		
		;16bit dec zp_map_addr by 32
		lda zp_map_addr
		sec
		sbc #32*1
		sta zp_map_addr
		bcs:+
			dec zp_map_addr+1
		:
		
		;get attribute buffer offset
		lda zp_camera_x
		alr #%11111000		;and #i, then lsr
		lsr
		adc #256-2
		sta zp_attr_buf_offset
		
		ldx #8
		:
			;blend new data w/ buffered data
			ldy #30
			lda (zp_map_addr), Y
			and #%00110011
			sta zp_temp
			ldy zp_attr_buf_offset	
			lda attribute_buffer, Y
			and #%11001100
			ora zp_temp
			sta zp_tile_queue+30-1, X
			dex
			
			ldy #31
			lda (zp_map_addr), Y
			and #%00110011
			sta zp_temp
			ldy zp_attr_buf_offset	
			lda attribute_buffer+1, Y
			and #%11001100
			ora zp_temp
			sta zp_tile_queue+30-1, X
			
			;16bit inc zp_map_addr by 32
			lda zp_map_addr
			clc
			adc #32
			sta zp_map_addr
			bcc:+
				inc zp_map_addr+1
			:
			
			;inc attribute buffer offset
			lda zp_attr_buf_offset
			clc
			adc #2
			;and #%00111111
			sta zp_attr_buf_offset
			
			dex
		bne:--
		
		jmp skip_draw_get
		
draw_left:
		;get PPU draw address
		lda zp_camera_x
		lsr
		lsr
		lsr
		sta zp_column_low
		;high byte is either $20 or $28
		lda #$20
		sta zp_column_high
		
		;get offset for reading map
		lda zp_camera_x
		sta zp_map_offset
		lda zp_camera_x+1
		sec
		sbc #0;2;4?	;read left side not right?
		sta zp_map_offset+1
		
		;16bit asl twice
		asl zp_map_offset
		rol zp_map_offset+1
		asl zp_map_offset
		lda zp_map_offset+1
		rol
		and #%00000111			;limit to 8 pages
		sta zp_map_offset+1
		
		clc
		lda #<Map
		adc zp_map_offset
		sta zp_map_addr
		lda #>Map
		adc zp_map_offset+1
		sta zp_map_addr+1
		
		;ldy zp_map_offset
		;tay
		ldy #0
		ldx #30
		:
			lda (zp_map_addr), Y
			sta zp_tile_queue-1, X
			iny
			dex
		bne:-
		
		;check for attribute update
		lda zp_camera_x
		and #%00011111
		cmp #0+8;*2;*3
		beq new_attr_left_easy
		bcc:+
			cmp #16
			bcc new_attr_left_easy
		:
		cmp #16+8;*2;*3
		beq new_attr_left_hard
		bcc:+
			cmp #32
			bcc new_attr_left_hard
		:
		jmp skip_draw_get
		
new_attr_left_easy:
		;flag update
		lda zp_PPU_update
		and #%01111111
		sta zp_PPU_update
		
		;get high byte from nametable
		lda zp_column_high
		clc
		adc #3
		sta zp_attr_addr_high
		
		lda zp_camera_x
		rol
		rol
		rol
		rol
		anc #%00000111	;clc
		adc #$C0
		sta zp_attr_addr_low	;= attribute base + (scroll / 32)
		
		
		;16bit dec zp_map_addr by 32*3
		lda zp_map_addr
		sec
		sbc #32*1
		sta zp_map_addr
		bcs:+
			dec zp_map_addr+1
		:
		
		;get attribute buffer offset
		lda zp_camera_x
		alr #%11111000		;and #i, then lsr
		lsr					;carry clear
		adc #256-2
		sta zp_attr_buf_offset
		
		ldx #8
		:
			;update tile queue and buffer
			ldy #30						;queue update
			lda (zp_map_addr), Y
			sta zp_tile_queue+30-1, X
			ldy zp_attr_buf_offset		;buffer update
			sta attribute_buffer, Y
			dex							;step
			ldy #31						;queue update
			lda (zp_map_addr), Y
			sta zp_tile_queue+30-1, X
			ldy zp_attr_buf_offset		;buffer update
			sta attribute_buffer+1, Y
			
			;16bit inc zp_map_addr by 32
			lda zp_map_addr
			clc
			adc #32
			sta zp_map_addr
			bcc:+
				inc zp_map_addr+1
			:
			
			;inc attribute buffer offset
			lda zp_attr_buf_offset
			clc
			adc #2
			;and #%00111111
			sta zp_attr_buf_offset
			
			dex							;step
		bne:--
		
		jmp skip_draw_get
		
		
new_attr_left_hard:
		;flag update
		lda zp_PPU_update
		and #%01111111
		sta zp_PPU_update
		
		;get high byte from nametable
		lda zp_column_high
		clc
		adc #3
		sta zp_attr_addr_high
		
		lda zp_camera_x
		rol
		rol
		rol
		rol
		anc #%00000111	;clc
		adc #$C0
		sta zp_attr_addr_low	;= attribute base + (scroll / 32)
		
		
		;16bit dec zp_map_addr by 32
		lda zp_map_addr
		sec
		sbc #32*3
		sta zp_map_addr
		bcs:+
			dec zp_map_addr+1
		:
		
		;get attribute buffer offset
		lda zp_camera_x
		alr #%11111000		;and #i, then lsr
		lsr					;carry clear
		adc #256-6
		sta zp_attr_buf_offset
		
		ldx #8
		:
			;blend new data w/ buffered data
			ldy #30
			lda (zp_map_addr), Y
			and #%11001100
			sta zp_temp
			ldy zp_attr_buf_offset	
			lda attribute_buffer, Y
			and #%00110011
			ora zp_temp
			sta zp_tile_queue+30-1, X
			dex
			
			ldy #31
			lda (zp_map_addr), Y
			and #%11001100
			sta zp_temp
			ldy zp_attr_buf_offset	
			lda attribute_buffer+1, Y
			and #%00110011
			ora zp_temp
			sta zp_tile_queue+30-1, X
			
			;16bit inc zp_map_addr by 32
			lda zp_map_addr
			clc
			adc #32
			sta zp_map_addr
			bcc:+
				inc zp_map_addr+1
			:
			
			;inc attribute buffer offset
			lda zp_attr_buf_offset
			clc
			adc #2
			;and #%00111111
			sta zp_attr_buf_offset
			
			dex
		bne:--
		
		jmp skip_draw_get
		
skip_draw_get:

;reset camera before inputs
	lda #0
	sta zp_camera_speed
	
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
	beq r_button_end
		;inc player_x
		lda #1
		sta zp_camera_speed
r_button_end:

	lda zp_buttons
	and #BUTTON_LEFT
	beq:+
		;dec player_x
		lda #$FF
		sta zp_camera_speed
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

;	count .set 0
;.repeat player_count
	sta $0208+4*0;+count*16
	sta $0208+4*1;+count*16
	adc #15
	sta $0208+4*2;+count*16
	sta $0208+4*3;+count*16
	;adc #7
;	count .set count +1
;.endrepeat
	
	;write new x position to OAM mirror
;	count .set 0
;.repeat player_count
	lda player_x
	sta $0208+3+4*0;+count*16
	sta $0208+3+4*2;+count*16
	adc #8
	sta $0208+3+4*1;+count*16
	sta $0208+3+4*3;+count*16
;	count .set count +1
;.endrepeat
	
	
	lda zp_buttons
	sta zp_previous_buttons
	
	
	;pull preserved registers from stack
	pla
	tay
	pla
	tax
	pla
	
	bit $2002
	bit $2002
	
	jmp end_of_vblank