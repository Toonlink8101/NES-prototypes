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

;Constants for function variables
	divider 	= 0
	counter 	= 1
	volume 		= 2
	lfsr 		= 3
	lfsr_tap	= 4


.segment "CODE"
Iterate_channels:
;Waveform Select Jump
	; 4+25 cycles
	ldy #0			; no need to clear Y, since first channel overrides it w/o reading Y
	;clc				; remove. Only needed for one 
	ldx waveforms
	lda jump_table_lo,x
	pha
	lda jump_table_hi,x
	pha
	;pull return address from stack
	rts


;Generate massive table of possible channel combos
.segment "CODE"
;Define each output function
.macro wave0
.local reset_end
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
				jmp reset_end
			:
			;if state was high
				clc
				inc lfsr+channel
				lda lfsr_tap+channel
	reset_end:
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
		sbc volume+channel		; fix! set C
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
.local read_high
.local output
	; Generate Page Sample
		;samples are raw 4-bit, the sound engine must also manually end playback either with a new waveform or a blank sample
		; divider holds state. counter holds low byte of sample address, while volume holds the high byte
		; sample loops through a full page of data (512 samples)
		; 31 - 35 cycles (assuming zeropage)
		; 22 bytes
		ldx #0
		asl divider+channel
		bcs read_high
	;read_low:
			lda (lfsr+channel, X)
			and #%00001111
			inc lfsr+channel
			jmp output
	read_high:
			inc divider+channel
			lda (lfsr+channel, X)
			lsr
			lsr
			lsr
			alr #$FE		;lsr, then clc
	output:
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
		;and #%01111110		;redundant
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
		.byte .lobyte(.ident(.sprintf("func%d%d%d%d", I, J, K, L)))
	.endrepeat
	.endrepeat
	.endrepeat
	.endrepeat
jump_table_hi:
	.repeat 4, I
	.repeat 4, J
	.repeat 4, K
	.repeat 4, L
		.byte .hibyte(.ident(.sprintf("func%d%d%d%d", I, J, K, L)))
	.endrepeat
	.endrepeat
	.endrepeat
	.endrepeat
	