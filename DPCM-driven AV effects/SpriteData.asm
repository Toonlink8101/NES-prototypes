.segment "RODATA"

.align 256

spritedata:
;Y, sprite num, attributes, X
;76543210
;||||||||
;||||||++- palette (4 to 7) of sprite
;|||+++--- Unimplemented
;||+------ priority (0: in front of background; 1: behind background)
;|+------- flip sprite horizontally
;+-------- flip sprite vertically

;buffer
	.byte $FF, $FF, $FF, $FF
	.byte $FF, $FF, $FF, $FF

.repeat player_count/2
;player
	.byte $FF, $08, $01, $60
	.byte $FF, $09, $01, $68
	.byte $FF, $18, $01, $60
	.byte $FF, $19, $01, $68
	.byte $FF, $28, $01, $60
	.byte $FF, $29, $01, $68
	.byte $FF, $38, $01, $60
	.byte $FF, $39, $01, $68
.endrepeat

;buffer
	.byte $FF, $FF, $FF, $FF
	.byte $FF, $FF, $FF, $FF	

;right arm
	;red
	.byte $80, $60, $00, $60
	.byte $80, $61, $00, $68
	.byte $88, $70, $00, $60
	.byte $88, $71, $00, $68
	;grey 
	.byte $89, $55, $03, $6B
	
;left arm
	;blue
	.byte $81, $30, $01, $85
	.byte $80, $31, $01, $8D
	.byte $88, $40, $01, $87
	.byte $88, $41, $01, $8F
	.byte $90, $50, $01, $84
	.byte $90, $51, $01, $8C
	;grey
	.byte $88, $56, $03, $7F
	
;body
	;legs
	.byte $90, $66, $03, $79
	.byte $97, $76, $03, $79
	.byte $97, $75, $03, $71
	
	;face
	.byte $76, $35, $03, $71
	.byte $77, $36, $03, $79
	.byte $7E, $45, $03, $71
	.byte $7F, $46, $03, $79

;"armor"
	;chestplate
	.byte $86, $53, $02, $77	;middle plate
	.byte $86, $52, $02, $6F	;right shoulder	
	.byte $8E, $62, $02, $6F
	.byte $8E, $63, $02, $77
	.byte $96, $72, $02, $6F
	.byte $96, $73, $02, $77
	.byte $81, $54, $02, $7F	;left shoulder
	
	
	;helmet
	.byte $6F, $32, $02, $70
	.byte $6F, $33, $02, $78
	.byte $71, $34, $02, $80
	.byte $77, $42, $02, $70
	.byte $77, $43, $02, $78
	.byte $79, $44, $02, $80
	
;highlights
	.byte $7B, $37, $01, $72
	.byte $78, $47, $81, $7B
	.byte $7C, $57, $80, $78
	
;bottom of red
	.byte $90, $80, $00, $60
	.byte $90, $81, $00, $68