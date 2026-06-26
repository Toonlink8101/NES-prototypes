; Figured I should have a seperate place to brainstrom waveform generation ideas

;Constants for function variables
	divider 	= 0
	counter 	= 1
	volume 		= 2
	lfsr 		= 3
	lfsr_tap	= 4
	
	chan1		= 0
	chan2		= 2
	chan3		= 4
	chan4		= 6

;Modulates the frequency of the APU's Pulse 1 channel
	; alternates between writing lfsr and lfsr_tap at the Timer's low 8-bits
	; The rate of oscillation is determined by divider and volume.
	; The divider functions as expected, while the volume triggers the other value when it equals counter.
	; This macro could easily be adapted to write to any APU register
	; Writing to Duty/Volume could be one function
.marco PulseFM channel
	; 16 - 21,25 cycles
	; ? bytes
	dec counter				;5
	beq @reset				;2,3 if taken
	lda volume				;3
	cmp counter				;3
	bne @end				;2,3 if taken
	lda lfsr				;3
	sta $4002 ;APU P1 pitch	;4
	jmp @end				;3
@reset:
	lda lfsr_tap			;3
	sta $4002 ;APU P1 pitch	;4
	lda divider				;3
	sta counter				;3
@end:
.endmacro