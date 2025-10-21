; This Code is at some point called during the IRQ routine, whenever the sound engine is interated

; This version cuts down the waveform count to four: Pulse, Linear (saw/tri), LFSR, and Sample (Page)
; All waveforms are simulateously selected by a jump table, where the table holds all 256 possible combinations of channels
; This takes up a sizable amount of memory (6-8k), but keeps execution quick without performance drawbacks.

; Since variables are used differently by each waveform, they are all stored in a section of 21 bytes,
; where 5 bytes are assigned to each channel, and the remaining byte selects the waveforms.
; For speed, all variables are stored on the zeropage

;Waveforms:
; 00 - Pulse
; 01 - Linear
; 10 - LFSR
; 11 - Sample

; Per channel optimizations:
; - skip "adc identity_table, y" when y=0 (first channel)
; - replace "tya, adc volume" w/ "lda volume" when y=0 (first channel)
; - skip "tay" when last channel (accumulator pushed to $4011 anyway)

.segment "ZEROPAGE"
	waveforms: .res 1
	channel_vars: .res 20

	.ifndef(preserve_A)
		preserve_A: .res 1
		preserve_X: .res 1
		preserve_Y: .res 1
	.endif

;Constants for function variables
	divider 	= 0
	counter 	= 1
	volume 		= 2
	lfsr 		= 3
	lfsr_tap	= 4


.segment "CODE"
Iterate_channels:
;Waveform Select Jump
	; 8+25 cycles
	ldy #0			; no need to clear Y, since first channel overrides it w/o reading Y
	clc				; not always needed, should be handled after jump, if at all
	ldx waveforms
	lda jump_table_lo,x
	pha
	lda jump_table_hi,x
	pha
	;pull return address from stack
	rts


;Generate massive table of possible channel combos
.segment "CODE80"
;Define each output function
.macro wave0
	; Generate Pulse Wave, optimized w/ noise vars (divider, counter, volume, lfsr, lfsr_tap)
		; lfsr stores state as a shift register. lfsr_tap holds high divider.
		; 14?,20? - 32?,34 cycles (assumes zeropage)
		; 18 bytes
		dec counter+channel
		bne:++
			;update state
			asl lfsr+channel
			;if state was low
			bcs:+
				lda divider+channel
				jmp @reset_end
			:
			;if state was high
				clc
				inc lfsr+channel
				lda lfsr_tap+channel
	@reset_end:
			sta counter+channel
		:
		bit lfsr+channel
		; if plus, add volume to output
		bmi:+
			tya
			adc volume+channel
			tay
		:
.endmacro

.macro wave1
	; Generate Linear Wave (Triangle/Sawtooth)
		; lfsr_tap holds falling value (+), while lfsr hold rising value (-).
		; Since their value is subtracted, they must be positive and negative, respectively
		; period ~=~ ciel(divider / lfsr) + ciel(divider / lfsr_tap)
		; setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
		; 28 - 33,35 (assumes zeropage)
		; 29 bytes
		lda counter+channel
		sbc volume+channel
		bvc:+
			ldx lfsr+channel
			stx volume+channel
			lda #0
		:
		sta counter+channel
		cmp divider+channel
		bcc:+
			ldx lfsr_tap+channel
			stx volume+channel
		:
		alr #%00111100
		lsr
		adc identity_table, y
		tay
.endmacro

.macro wave2
	; Generate thin lfsr
		; 8 - 24,34 cycles (assumes zeropage)
		; 20 bytes
		dec counter+channel
		bne:++
			; reset counter
			lda divider+channel
			sta counter+channel
			; galois lfsr
			lda lfsr_tap+channel
			sre lfsr+channel			;lsr var, then eor var
			bcs:+
				sta lfsr+channel
				tya
				adc volume+channel
				tay
			:
		:
.endmacro

.macro wave3
	; Generate Page Sample
		;samples are raw 4-bit, the sound engine must also manually end playback either with a new waveform or a blank sample
		; divider holds state. counter holds low byte of sample address, while volume holds the high byte
		; sample loops through a full page of data (512 samples)
		; 31 - 35 cycles (assuming zeropage)
		; 22 bytes
		ldx #0
		asl divider+channel
		bcs @read_high
	;read_low:
			lda (lfsr+channel, X)
			and #%00001111
			inc lfsr+channel
			jmp @output_volume
	@read_high:
			inc divider+channel
			lda (lfsr+channel, X)
			lsr
			lsr
			lsr
			alr #FE		;lsr, then clc
	@output_volume:
			adc identity_table, y
			tay
.endmacro

;Generate all combinations
	.repeat 4, I
	.repeat 4, J
	.repeat 4, K
	.repeat 4, L
	.ident(.sprintf("func%d%d%d%d", I, J, K, L)):
		channel .set 0*5
		.ident(.sprintf("wave%d", I))
		channel .set 1*5
		.ident(.sprintf("wave%d", J))
		channel .set 2*5
		.ident(.sprintf("wave%d", K))
		channel .set 3*5
		.ident(.sprintf("wave%d", L))
	;Output to DPCM
		; 8+2 cycles
		tya		; Could technically be removed if macros are changed (LFSR can't be, in this case)
		asl
		and #%01111110
		sta $4011
		
		; restore registers
		; 9 cycles
		lda preserve_A
		ldx preserve_X
		ldy preserve_Y

		rti
	.endrepeat
	.endrepeat
	.endrepeat
	.endrepeat



;Generate jump tables
.segment "RODATA"
jump_table_lo:
	.repeat 4, I
	.repeat 4, J
	.repeat 4, K
	.repeat 4, L
		.lobyte(.ident(.sprintf("func%d%d%d%d", I, J, K, L)))
	.endrepeat
	.endrepeat
	.endrepeat
	.endrepeat
jump_table_hi:
	.repeat 4, I
	.repeat 4, J
	.repeat 4, K
	.repeat 4, L
		.hibyte(.ident(.sprintf("func%d%d%d%d", I, J, K, L)))
	.endrepeat
	.endrepeat
	.endrepeat
	.endrepeat
	
.segment "RODATA"
.ifndef(identity_table)
	identity_table:
		.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f
		.byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f
		.byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f
		.byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f
		.byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f
		.byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f
		.byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f
		.byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f
		.byte $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f
		.byte $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f
		.byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af
		.byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
		.byte $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf
		.byte $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df
		.byte $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef
		.byte $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff
.endif