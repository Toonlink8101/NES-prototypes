Main:
;scroll handling
	;move camera
	lda zp_camera_x
	clc
	adc zp_camera_speed
	sta zp_camera_x
	bcc:+
		bit zp_camera_speed
		bmi:+
			inc zp_camera_x+1
	:
	lda zp_camera_x
	cmp #$FF
	bne:+
		bit zp_camera_speed
		bpl:+
			dec zp_camera_x+1
	:
	
	
	;bne:+
	;	inc zp_camera_x+1
	;:
	
	
	;check if PPU draw needed
	lda #0
	sta zp_PPU_update
	
	lda zp_camera_speed		;make sure camera is moving
	bne:+
		jmp skip_draw_get
	:
	;lda zp_camera_speed		;keep last non-zero camera speed
	;beq:+
	;	sta zp_prev_cam_speed
	;:
	;lda zp_camera_x			;make sure camera =/= 8x
	;and #%00000111
	;bne:+
	;	jmp skip_draw_get
	;:
	
	;flag PPU update
	lda #$FF
	sta zp_PPU_update
	
	lda zp_camera_x			;if (camera % 8 < 4), then draw left
	and #%00000111
	cmp #4
	bcs:+
		jmp draw_left
	:
	
	;lda zp_camera_speed
	;lda zp_prev_cam_speed
	;bpl:+
	;	jmp draw_left
	;:
		
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
		and #%11111000
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
		
right_attributes:
		
		;check for attribute update
		;lda zp_camera_x			;if (x % 8)+4 != 0, skip attributes
		;and #%00000111
		;eor #%00000100
		;bne:+
		;	jmp skip_draw_get
		;:
		
		;lda zp_camera_speed		;if moving left, skip attributes
		;bpl:+
		;	jmp skip_draw_get
		;:
		
		lda zp_camera_speed		;if moving left, do left attributes
		bpl:+
			jmp left_attributes
		:
		
		lda zp_camera_x
		and #%00011111
		cmp #0;+8-4;*2;*3
		bne:+
			jmp new_attr_right_hard
		:
		bcc:+
			cmp #16;-4
			bcs:+
				jmp new_attr_right_hard
		:
		cmp #16;+8-4;*2;*3
		bne:+
			jmp new_attr_right_easy
		:
		bcc:+
			cmp #32;-4
			bcs:+
				jmp new_attr_right_easy	
		:
		jmp skip_draw_get
		
draw_left:
		;get PPU draw address
		lda zp_camera_x
		and #%11111000
		lsr
		lsr
		lsr
		sta zp_column_low
		;high byte is either $20 or $28
		lda #$20
		sta zp_column_high
		
		;get offset for reading map
		lda zp_camera_x
		and #%11111000
		sta zp_map_offset
		lda zp_camera_x+1
		sec
		sbc #1;4;?	;read left side not right?
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
		;lda zp_camera_x			;if x % 8 != 0, skip attributes
		;and #%00000111
		;bne:+
		;	jmp skip_draw_get
		;:
		
		lda zp_camera_speed		;if moving right, do right attributes
		bmi:+
			jmp right_attributes
		:
		
left_attributes:
		
		lda zp_camera_x			;if x % 8 != 0, skip attributes
		and #%00000111
		bne:+
			jmp skip_draw_get
		:
		
		lda zp_camera_x
		and #%00011111
		cmp #0;+4;+8;*2;*3
		bne:+
			jmp new_attr_left_easy
		:
		bcc:+
			cmp #16;8+4;16
			bcs:+
				jmp new_attr_left_easy
		:
		cmp #16;+4;+8;*2;*3
		bne:+
			jmp new_attr_left_hard
		:
		bcc:+
			cmp #32;16+8+4;32
			bcs:+
				jmp new_attr_left_hard
		:
		jmp skip_draw_get
		
		
.include "AttributeMath.asm"


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