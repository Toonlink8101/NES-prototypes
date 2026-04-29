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

.repeat player_count
;player
	.byte $FF, $18, $01, $60
	.byte $FF, $1A, $01, $68
	.byte $FF, $38, $01, $60
	.byte $FF, $3A, $01, $68
.endrepeat

;buffer
	.byte $FF, $FF, $FF, $FF
	.byte $FF, $FF, $FF, $FF	

;TODO add column of black on right of screen drawn with OAM DMA
;right boarder
	count .set 0
.repeat 15
	.byte count, $00, $02, $F8
	count .set count+16
.endrepeat