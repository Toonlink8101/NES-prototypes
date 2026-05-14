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