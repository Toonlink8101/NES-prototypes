; This Code is at some point called during the IRQ routine, whenever the sound engine is interated

; This version has four waveforms: Pulse, Linear (saw/tri), LFSR, and Sample (Page)

;Instead of using a jump table of any kind, either for selecting individual waves or all of them, 
; this version simply uses an indirect jump to a pointer that takes the program to the next desired waveform.
; This save a significant amount of ROM at the cost of only a small amount of non-zeropage RAM.

;Honestly, I feel a little stupid that I didn't think of it earlier. It's such an obvious option.
; It costs fewer cycles than even just one RTS jump


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

; 7+31+5+35+5+35+5+33+3+6+9+6
; = 180 total cycles

.globalzp preserve_A, preserve_X, preserve_Y, DMC_output

.segment "BSS"
	waveforms: .res 8

.segment "ZEROPAGE"
	channel_vars: .res 20

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


.segment "CODE"
Iterate_channels:
;Indirect jump to first channel
; 7 cycles
	clc		; Still needed for Pulse and others. Also, must be accounted for in Linear
	jmp (waveforms+chan1)


;;; Channel 1
channel .set 0*5

Pulse_chan1:
; Generate Pulse Wave, optimized w/ noise vars (divider, counter, volume, lfsr, lfsr_tap)
	; lfsr stores state as a shift register. lfsr_tap holds high divider.
	; 14?,20? - 32?,28 cycles (assumes zeropage)
	; 18? bytes
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
		;tya
		;adc volume+channel
		lda volume
		tay
	:
	jmp (waveforms+chan2)


Linear_chan1:
; Generate Linear Wave (Triangle/Sawtooth)
	; lfsr_tap holds falling value (+), while lfsr hold rising value (-).
	; Since their value is subtracted, they must be positive and negative, respectively
	; period ~=~ ciel(divider / lfsr) + ciel(divider / lfsr_tap)
	; setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
	; 28? - 33?,31 (assumes zeropage)
	; 29? bytes
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
	;adc identity_table, y
	tay
	jmp (waveforms+chan2)


LFSR_chan1:
; Generate thin lfsr
	; 8? - 24?,28 cycles (assumes zeropage)
	; 20? bytes
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
			;tya
			;adc volume+channel
			lda volume
			tay
		:
	:
	jmp (waveforms+chan2)


Sample_chan1:
; Generate Page Sample
	;samples are raw 4-bit, the sound engine must also manually end playback either with a new waveform or a blank sample
	; divider holds state. counter holds low byte of sample address, while volume holds the high byte
	; sample loops through a full page of data (512 samples)
	; 31? - 31 cycles (assuming zeropage)
	; 22? bytes
	ldx #0
	asl divider+channel
	bcs @read_high
;read_low:
		lda (lfsr+channel, X)
		and #%00001111
		inc lfsr+channel
		jmp @output
@read_high:
		inc divider+channel
		lda (lfsr+channel, X)
		lsr
		lsr
		lsr
		alr #$FE		;lsr, then clc
@output:
	;adc identity_table, y
	tay
	jmp (waveforms+chan2)


;;; Channel 2
channel .set 1*5

Pulse_chan2:
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
	jmp (waveforms+chan3)


Linear_chan2:
; Generate Linear Wave (Triangle/Sawtooth)
	; lfsr_tap holds falling value (+), while lfsr hold rising value (-).
	; Since their value is subtracted, they must be positive and negative, respectively
	; period ~=~ ciel(divider / lfsr) + ciel(divider / lfsr_tap)
	; setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
	; 28 - 33,35 (assumes zeropage)
	; 29 bytes
	lda counter+channel
	sbc volume+channel		; fix! set C
	bpl:+									;TODO: change all to match this!!!
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
	jmp (waveforms+chan3)


LFSR_chan2:
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
	jmp (waveforms+chan3)


Sample_chan2:
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
		jmp @output
@read_high:
		inc divider+channel
		lda (lfsr+channel, X)
		lsr
		lsr
		lsr
		alr #$FE		;lsr, then clc
@output:
	adc identity_table, y
	tay
	jmp (waveforms+chan3)


;;; Channel 3
channel .set 2*5

Pulse_chan3:
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
	jmp (waveforms+chan4)

Linear_chan3:
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
	jmp (waveforms+chan4)


LFSR_chan3:
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
	jmp (waveforms+chan4)


Sample_chan3:
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
		jmp @output
@read_high:
		inc divider+channel
		lda (lfsr+channel, X)
		lsr
		lsr
		lsr
		alr #$FE		;lsr, then clc
@output:
	adc identity_table, y
	tay
	jmp (waveforms+chan4)


;;; Channel 4
channel .set 3*5

Pulse_chan4:
; Generate Pulse Wave, optimized w/ noise vars (divider, counter, volume, lfsr, lfsr_tap)
	; lfsr stores state as a shift register. lfsr_tap holds high divider.
	; 14?,20? - 32?,34 cycles (assumes zeropage)
	; 18? bytes
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
	tya
	bit lfsr+channel
	; if plus, add volume to output
	bmi:+
		adc volume+channel
		;tay
	:
	jmp Output_to_DPCM

Linear_chan4:
; Generate Linear Wave (Triangle/Sawtooth)
	; lfsr_tap holds falling value (+), while lfsr hold rising value (-).
	; Since their value is subtracted, they must be positive and negative, respectively
	; period ~=~ ciel(divider / lfsr) + ciel(divider / lfsr_tap)
	; setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
	; 28? - 33?,33 (assumes zeropage)
	; 29? bytes
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
	;tay
	jmp Output_to_DPCM


LFSR_chan4:
; Generate thin lfsr
	; 8? - 24?,32 (w/o jump) cycles (assumes zeropage)
	; 20? bytes
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
			jmp Output_to_DPCM
		:
	:
	tya
	jmp Output_to_DPCM

Sample_chan4:
; Generate Page Sample
	;samples are raw 4-bit, the sound engine must also manually end playback either with a new waveform or a blank sample
	; divider holds state. counter holds low byte of sample address, while volume holds the high byte
	; sample loops through a full page of data (512 samples)
	; 31? - 33 cycles (assuming zeropage)
	; 22? bytes
	ldx #0
	asl divider+channel
	bcs @read_high
;read_low:
		lda (lfsr+channel, X)
		and #%00001111
		inc lfsr+channel
		jmp @output
@read_high:
		inc divider+channel
		lda (lfsr+channel, X)
		lsr
		lsr
		lsr
		alr #$FE		;lsr, then clc
@output:
	adc identity_table, y
	;tay
	jmp Output_to_DPCM


	;fall through?
	; The slowest one should, likely


Output_to_DPCM:
;Output to DPCM
	; 6+2 cycles
	asl
	;and %01111110	;shouldn't be necessary
	sta DMC_output
	
	; restore registers
	; 9 cycles
	lda preserve_A
	ldx preserve_X
	ldy preserve_Y

	; 6 cycles
	rti