; This Code is at some point called during the IRQ routine, whenever the sound engine is interated

; This version has four waveforms: Pulse, Linear (saw/tri), LFSR, and Sample (Page)

;Instead of using a jump table of any kind, either for selecting individual waves or all of them, 
; this version simply uses an indirect jump to a pointer that takes the program to the next desired waveform.
; This save a significant amount of ROM at the cost of only a small amount of non-zeropage RAM.

;Honestly, I feel a little stupid that I didn't think of it earlier. It's such an obvious option.
; It costs fewer cycles than even just one RTS jump


; Per channel optimizations:
; - skip "adc identity_table, y" when y=0 (first channel)
; - replace "tya, adc volume" w/ "lda volume" when y=0 (first channel)
; - skip "tay" when last channel (accumulator pushed to $4011 anyway)


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


;Generate Pulse Wave
	; adds output to Y, trashes A
	; lfsr stores state as a shift register. Whenever bit7 is set, volume will be aded to Y. $AA or $55 will oscillate normally
	; divider and lfsr_tap hold the duration of the silent and outputed segments respectively.
	; counter is the time left until the state (lfsr) is updated, whereupon it is reset to either divider or lfsr_tap. Can be treated like phase
.macro Pulse channel
	; 8 - 23,28 before output logic, 6 - 12 for output
	; 14,20 - 34,35 cycles
	; 18 bytes?
	dec counter+channel*5+channel_vars			;5
	bne:++										;2,3 if taken
		;update state
		asl lfsr+channel*5+channel_vars			;5
		;if state was low
		bcs:+									;2,3 if taken
			lda divider+channel*5+channel_vars	;3
			jmp @reset_end						;3
		:
		;if state was high
			clc									;2
			inc lfsr+channel*5+channel_vars		;5
			lda lfsr_tap+channel*5+channel_vars	;3
@reset_end:
		sta counter+channel*5+channel_vars		;3
	:
	bit lfsr+channel*5+channel_vars				;3
	; if plus, add volume to output
	bmi:+										;2,3 if taken
		tya										;2
		adc volume+channel*5+channel_vars		;3
		tay										;2
	:
.endmacro

;Generate Pulse Wave Optimized
	; adds output to Y, trashes A and X
	; lfsr stores state, but can be set to anything. Whenever bit7 is set, volume will be aded to Y.
	; divider and lfsr_tap hold the duration of the silent and outputed segments respectively.
	; counter is the time left until the state (lfsr) is updated, whereupon it is reset to either divider or lfsr_tap. Can be treated like phase
.macro PulseOpt channel
	; 14?,20? - 32?,34 cycles
	; 8 - 23,21 before output logic, 6 - 12 for output
	; 14,20 - 27,35 cycles
	; ?? bytes
	dec counter+channel*5+channel_vars			;5
	bne:++										;2,3 if taken
		;update state
		bit lfsr+channel*5+channel_vars			;3
		;if state was low
		bmi:+									;2,3 if taken
			ldx #$FF							;2
			lda divider+channel*5+channel_vars	;3
			jmp @reset_end						;3
		:
		;if state was high
			ldx #0								;2
			lda lfsr_tap+channel*5+channel_vars	;3
@reset_end:
		sta counter+channel*5+channel_vars		;3
		stx lfsr								; forgot this :P
	:
	bit lfsr+channel*5+channel_vars				;3
	; if plus, add volume to output
	bmi:+										;2,3 if taken
		tya										;2
		adc volume+channel*5+channel_vars		;3
		tay										;2
	:
.endmacro

;Generate Pulse Wave Optimized again
	; adds output to Y, trashes A
	; lfsr stores state, but can be set to anything. Whenever bit7 is set, volume will be aded to Y.
	; divider and lfsr_tap hold the duration of the silent and outputed segments respectively.
	; counter is the time left until the state (lfsr) is updated, whereupon it is reset to either divider or lfsr_tap. Can be treated like phase
.macro PulseOpt2 channel
	; ?????14,20 - 24,30 cycles
	; ?? bytes
	dec counter+channel*5+channel_vars			;5
	bne:++										;2,3 if taken
		;update state
		bit lfsr+channel*5+channel_vars			;3
		;if state was low
		bmi:+									;2,3 if taken
			lda divider+channel*5+channel_vars	;3
			sta counter+channel*5+channel_vars	;3
			lda #$FF							;2
			sta lfsr
			bne @output							;3
		:
		;if state was high
		lda lfsr_tap+channel*5+channel_vars		;3
		sta counter+channel*5+channel_vars		;3
		lda #0									;2
		sta lfsr
		beq:++	;jump to end					;3
	:
	bit lfsr+channel*5+channel_vars				;3
	; if plus, add volume to output
	bmi:+										;2,3 if taken
@output:
		tya										;2
		adc volume+channel*5+channel_vars		;3
		tay										;2
	:
.endmacro

;Generate Pulse Wave Optimized, but keeping shift register
	; adds output to Y, trashes A
	; lfsr stores state as a shift register. Whenever bit7 is set, volume will be aded to Y. $AA or $55 will oscillate normally
	; divider and lfsr_tap hold the duration of the silent and outputed segments respectively.
	; counter is the time left until the state (lfsr) is updated, whereupon it is reset to either divider or lfsr_tap. Can be treated like phase
.macro PulseOpt3 channel
	; 8 - 23,28 before output logic, 6 - 12 for output
	; 14,20 - 30,31 cycles
	; 18 bytes?
	dec counter+channel*5+channel_vars			;5
	bne:++										;2,3 if taken
		;update state
		asl lfsr+channel*5+channel_vars			;5
		;if state was low
		bcs:+									;2,3 if taken
			lda divider+channel*5+channel_vars	;3
			sta counter+channel*5+channel_vars	;3
			bcc @output							;3
		:
		;if state was high
		inc lfsr+channel*5+channel_vars			;5
		lda lfsr_tap+channel*5+channel_vars		;3
		sta counter+channel*5+channel_vars		;3
		clc										;2
		bcc:++	;jump to end					;3
	:
	bit lfsr+channel*5+channel_vars				;3
	; if plus, add volume to output
	bmi:+										;2,3 if taken
@output:
		tya										;2
		adc volume+channel*5+channel_vars		;3
		tay										;2
	:
.endmacro


;Generate Linear Wave
	;trashes X, leaves output in A
	; Generates triangles, saws, and anything inbetween
	; counter holds the output value
	; volume is added to counter each iteration
	; divider is the "ceiling" for counter. Once counter passes this value, counter is set to lfsr_tap
	; lfsr_tap holds the decreasing value (-). It's loaded into volume when counter passes divider
	; lfsr hold the increasing value (+). It's loaded into volume when counter underflows
	; The period of this generator is roughly as follows: Period = ceil(divider / lfsr) + ciel(divider / lfsr_tap)
	; Setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
.macro Linear channel
	; 28 - 33,35 cycles?
	; 29 bytes
	lda counter+channel*5+channel_vars
	adc volume+channel*5+channel_vars
	bpl:+
		ldx lfsr+channel*5+channel_vars
		stx volume+channel*5+channel_vars
		lda #8
	:
	sta counter+channel*5+channel_vars
	cmp divider+channel*5+channel_vars
	bcc:+
		ldx lfsr_tap+channel*5+channel_vars
		stx volume+channel*5+channel_vars
	:
	alr #%00111100
	lsr
	;adc identity_table, y
	;tay
.endmacro


;Generate LFSR Wave
	; trashes A, adds output to Y
	;When counter hits zero, the LFSR state is iterated
	; divider is the value counter is reset to
	; naturally, lfsr_tap holds the tap for the LFSR, and lfsr holds the current lfsr state
.macro LFSR channel
	; 16,22 - 24,34 cycles
	; 36 bytes
	dec counter+channel*5+channel_vars
	bne:++
		; reset counter
		lda divider+channel*5+channel_vars
		sta counter+channel*5+channel_vars
		; galois lfsr
		lda lfsr_tap+channel*5+channel_vars
		sre lfsr+channel*5+channel_vars			;lsr var, then eor var
		bcs:+
			sta lfsr+channel*5+channel_vars
			tya
			adc volume+channel*5+channel_vars
			tay
		:
		jmp:++
	:
	lda lfsr+channel*5+channel_vars
	and #%00000001
	bne:+
		tya
		adc volume+channel*5+channel_vars
		tay
	:
.endmacro

;Generate LFSR Wave w/ simplified pitch control
	; trashes A, adds output to Y
	; counter is a shift register. Whenever a 0 is shifted out, the lfsr updates
	; naturally, lfsr_tap holds the tap for the LFSR, and lfsr holds the current lfsr state
.macro LFSRsimple channel
	; 21,27 - 21,30 cycles
	; ?? bytes
	asl counter+channel*5+channel_vars			;5
	bcs:++										;2,3
		; galois lfsr
		lda lfsr_tap+channel*5+channel_vars		;3
		sre lfsr+channel*5+channel_vars			;5		;lsr var, then eor var
		bcs:+									;2,3
			sta lfsr+channel*5+channel_vars		;3
			tya									;2
			adc volume+channel*5+channel_vars	;3
			tay									;2
		:
		jmp:++									;3
	:
	inc counter+channel*5+channel_vars			;5
	lda lfsr+channel*5+channel_vars				;3
	and #%00000001								;2
	bne:+										;2,3
		tya										;2
		adc volume+channel*5+channel_vars		;3
		tay										;2
	:
.endmacro


;Generate Sample
	; samples are 4-bit, with two stored in each byte
	; Playback will endlessly loop through a full page (256 bytes/512 samples) until manually stopped by changing the waveform
	; divider holds state as a shift register. When bit7 is clear, the low nibble is output. 
		; Otherwise, the high nibble is output and counter is incremented
		; Loading $AA into divider plays samples normally. $00 will skip every other sample. $EE will halve(?) playback speed
	; lfsr holds the low byte of the sample address. 
	; lfsr_tap holds the high byte of the sample address.
.macro Sample channel
	; 31 - 35 cycles
	; 22 bytes
	ldx #0
	asl divider+channel*5+channel_vars
	bcs @read_high
;read_low:
		lda (lfsr+channel*5+channel_vars, X)
		and #%00001111
		inc lfsr+channel*5+channel_vars
		jmp @output
@read_high:
		inc divider+channel*5+channel_vars
		lda (lfsr+channel*5+channel_vars, X)
		lsr
		lsr
		lsr
		alr #$FE		;lsr, then clc
@output:
	adc identity_table, y
	tay
.endmacro

;Generate Sample Optimized
	; trashes X, leaves output in A
	; samples are 4-bit, with two stored in each byte
	; Playback will endlessly loop through a full page (256 bytes/512 samples) until manually stopped by changing the waveform
	; divider holds state as a shift register. When bit7 is clear, the low nibble is output. 
		; Otherwise, the high nibble is output and counter is incremented
		; Loading $55 into divider plays samples normally. $FF will skip every other sample. $11 will halve(?) playback speed
	; lfsr holds the low byte of the sample address. 
	; lfsr_tap holds the high byte of the sample address.
.macro SampleOpt channel
	; 24 - 28 cycles
	; ?? bytes
	ldx #0										;2
	asl divider+channel*5+channel_vars			;5
	bcs @read_low								;2, 3 if taken
;read_high:
		lda (lfsr+channel*5+channel_vars, X)	;6
		tax										;2
		lda divide_by_16, X						;4
		jmp @output								;3
@read_low:
		lda (lfsr+channel*5+channel_vars, X)	;6
		anc #%00001111		;clears carry		;2
		inc lfsr+channel*5+channel_vars			;5
		inc divider+channel*5+channel_vars		;5
@output:
	;adc identity_table, y							;4
	;tay											;2
.endmacro

;Generate Sample Optimized again
	; trashes X, leaves output in A
	; samples are 4-bit, with two stored in each byte
	; Playback will endlessly loop through a full page (256 bytes/512 samples) until manually stopped by changing the waveform
	; divider holds state. When bit7 is clear, the low nibble is output. 
		; Otherwise, the high nibble is output and counter is incremented
		; As long as bit7 is initialize to 0 in divider, samples will play normally
	; lfsr holds the low byte of the sample address. 
	; lfsr_tap holds the high byte of the sample address.
.macro SampleOpt2 channel
	; 26 - 27 cycles
	; ?? bytes
	ldx #0										;2
	bit divider+channel*5+channel_vars			;3
	bmi @read_low								;2, 3 if taken
;read_high:
		lda (lfsr+channel*5+channel_vars, X)	;6
		tax										;2
		lda divide_by_16, X						;4
		ldx #$FF								;2
		jmp @output								;3
@read_low:
		lda (lfsr+channel*5+channel_vars, X)	;6
		anc #%00001111		;clears carry		;2
		inc lfsr+channel*5+channel_vars			;5
		ldx #$00								;2
@output:
	stx divider+channel*5+channel_vars			;3
	;adc identity_table, y							;4
	;tay											;2
.endmacro

;Generate 1 bit sample
	; volume controlable
	; lfsr holds next address low byte. Overflows to the start of the page (256*8= 2048 bits / 64 samples per sec = 32 seconds of 1 bit audio) O.O
	; lfsr_tap holds next address high byte
	; divider holds state as a shift register. Must init to %10000000 to avoid delay
	; counter holds current byte
.macro Sample1bit channel
	; 16,22 - 36,42 cycles
	; ?? bytes
	asl counter+channel*5+channel_vars			;5
	bcc:+										;2,3
		tya										;2
		adc volume+channel*5+channel_vars		;3
		tay										;2
	:
	asl divider+channel*5+channel_vars			;5
	bcc:+										;2,3
		inc divider+channel*5+channel_vars		;5
		ldx #0									;2
		lda (lfsr+channel*5+channel_vars, X)	;6
		sta counter+channel*5+channel_vars		;3
		inc lfsr+channel*5+channel_vars			;5
	:
.endmacro

; generates volume controlled samples, 3 volume states (full, half, 4th)
.macro Sample3vol channel
	; 24 - 28 cycles before volume 6,10,11 cycles during
	; ?? bytes
	ldx #0										;2
	asl divider+channel*5+channel_vars			;5
	bcs @read_low								;2, 3 if taken
;read_high:
		lda (lfsr+channel*5+channel_vars, X)	;6
		tax										;2
		lda divide_by_16, X						;4
		jmp @output								;3
@read_low:
		lda (lfsr+channel*5+channel_vars, X)	;6
		anc #%00001111		;clears carry		;2
		inc lfsr+channel*5+channel_vars			;5
		inc divider+channel*5+channel_vars		;5
@output:
	bit volume									;3
	bpl:+										;2,3
		lsr										;2
		bvc:+									;2,3
			lsr									;2
		:
	:
	;adc identity_table, y							;4
	;tay											;2
.endmacro

; generates volume controlled samples, 4 volume states (full, half, 4th, 8th)
.macro Sample4vol channel
	; 24 - 28 cycles before volume and 9,10,12,13 cycles during
	; ?? bytes
	ldx #0										;2
	asl divider+channel*5+channel_vars			;5
	bcs @read_low								;2, 3 if taken
;read_high:
		lda (lfsr+channel*5+channel_vars, X)	;6
		tax										;2
		lda divide_by_16, X						;4
		jmp @output								;3
@read_low:
		lda (lfsr+channel*5+channel_vars, X)	;6
		anc #%00001111		;clears carry		;2
		inc lfsr+channel*5+channel_vars			;5
		inc divider+channel*5+channel_vars		;5
@output:
	bit volume								;3
	bmi:+									;2,3
		lsr									;2
	:
	bvs:+									;2,3
		lsr									;2
		lsr									;2
	:
	;adc identity_table, y							;4
	;tay											;2
.endmacro

; generates volume controlled samples, 2 volume states (half, 4th)
.macro Sample2vol channel
	; 24 - 28 cycles before volume and 8,9 cycles during
	; ?? bytes
	ldx #0										;2
	asl divider+channel*5+channel_vars			;5
	bcs @read_low								;2, 3 if taken
;read_high:
		lda (lfsr+channel*5+channel_vars, X)	;6
		tax										;2
		lda divide_by_16, X						;4
		jmp @output								;3
@read_low:
		lda (lfsr+channel*5+channel_vars, X)	;6
		anc #%00001111		;clears carry		;2
		inc lfsr+channel*5+channel_vars			;5
		inc divider+channel*5+channel_vars		;5
@output:
	bit volume								;3
	lsr										;2
	bmi:+									;2,3
		lsr									;2
	:
	;adc identity_table, y							;4
	;tay											;2
.endmacro





; Iterate software channels
	; trashes A,X,Y
	; returns with output in A
.segment "CODE"
Iterate_channels:
;clear flags and registers
	clc
	lda #0		;not sure why clearing registers is necessary here...
	tax
	tay
	
;Indirect jump to first channel
; 7 cycles
	jmp (waveforms+chan1)


;;; Channel 1
channel .set 0

Pulse_chan1:
; Generate Pulse Wave, optimized w/ noise vars (divider, counter, volume, lfsr, lfsr_tap)
	; lfsr stores state as a shift register. lfsr_tap holds high divider.
	; 14?,20? - 32?,28 cycles (assumes zeropage)
	; 18? bytes
	dec counter+channel*5+channel_vars
	bne:++
		;update state
		asl lfsr+channel*5+channel_vars
		;if state was low
		bcs:+
			lda divider+channel*5+channel_vars
			jmp @reset_end
		:
		;if state was high
			clc
			inc lfsr+channel*5+channel_vars
			lda lfsr_tap+channel*5+channel_vars
@reset_end:
		sta counter+channel*5+channel_vars
	:
	bit lfsr+channel*5+channel_vars
	; if plus, add volume to output
	bmi:+
		;tya
		;adc volume+channel*5+channel_vars
		lda volume+channel*5+channel_vars
		tay
	:
	jmp (waveforms+chan2)


Linear_chan1:
; Generate Linear Wave (Triangle/Sawtooth)
	; lfsr_tap holds falling value (-), while lfsr hold rising value (+).
	; period ~=~ ciel(divider / lfsr) + ciel(divider / lfsr_tap)
	; setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
	; 28 - 33,35 (assumes zeropage)
	; 29 bytes
	lda counter+channel*5+channel_vars
	adc volume+channel*5+channel_vars
	bpl:+
		ldx lfsr+channel*5+channel_vars
		stx volume+channel*5+channel_vars
		lda #8
	:
	sta counter+channel*5+channel_vars
	cmp divider+channel*5+channel_vars
	bcc:+
		ldx lfsr_tap+channel*5+channel_vars
		stx volume+channel*5+channel_vars
	:
	alr #%00111100
	lsr
	;adc identity_table, y
	tay
	jmp (waveforms+chan2)


LFSR_chan1:
; Generate LFSR
	; 16,20 - 24,32 cycles (assumes zeropage)
	; 36? bytes
	dec counter+channel*5+channel_vars
	bne:++
		; reset counter
		lda divider+channel*5+channel_vars
		sta counter+channel*5+channel_vars
		; galois lfsr
		lda lfsr_tap+channel*5+channel_vars
		sre lfsr+channel*5+channel_vars			;lsr var, then eor var
		bcs:+
			sta lfsr+channel*5+channel_vars
			;tya
			;adc volume+channel*5+channel_vars
			lda volume+channel*5+channel_vars
			tay
		:
		jmp (waveforms+chan2)	;TODO: copy this elsewhere
	:
	lda lfsr+channel*5+channel_vars
	and #%00000001
	bne:+
		;tya
		;adc volume+channel*5+channel_vars
		lda volume+channel*5+channel_vars
		tay
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
	asl divider+channel*5+channel_vars
	bcs @read_high
;read_low:
		lda (lfsr+channel*5+channel_vars, X)
		and #%00001111
		inc lfsr+channel*5+channel_vars
		jmp @output
@read_high:
		inc divider+channel*5+channel_vars
		lda (lfsr+channel*5+channel_vars, X)
		lsr
		lsr
		lsr
		alr #$FE		;lsr, then clc
@output:
	;adc identity_table, y
	tay
	jmp (waveforms+chan2)


;;; Channel 2
channel .set 1

Pulse_chan2:
; Generate Pulse Wave, optimized w/ noise vars (divider, counter, volume, lfsr, lfsr_tap)
	; lfsr stores state as a shift register. lfsr_tap holds high divider.
	; 14?,20? - 32?,34 cycles (assumes zeropage)
	; 18 bytes
	dec counter+channel*5+channel_vars
	bne:++
		;update state
		asl lfsr+channel*5+channel_vars
		;if state was low
		bcs:+
			lda divider+channel*5+channel_vars
			jmp @reset_end
		:
		;if state was high
			clc
			inc lfsr+channel*5+channel_vars
			lda lfsr_tap+channel*5+channel_vars
@reset_end:
		sta counter+channel*5+channel_vars
	:
	bit lfsr+channel*5+channel_vars
	; if plus, add volume to output
	bmi:+
		tya
		adc volume+channel*5+channel_vars
		tay
	:
	jmp (waveforms+chan3)


Linear_chan2:
; Generate Linear Wave (Triangle/Sawtooth)
	; lfsr_tap holds falling value (-), while lfsr hold rising value (+).
	; period ~=~ ciel(divider / lfsr) + ciel(divider / lfsr_tap)
	; setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
	; 28 - 33,35 (assumes zeropage)
	; 29 bytes
	lda counter+channel*5+channel_vars
	adc volume+channel*5+channel_vars
	bpl:+
		ldx lfsr+channel*5+channel_vars
		stx volume+channel*5+channel_vars
		lda #8
	:
	sta counter+channel*5+channel_vars
	cmp divider+channel*5+channel_vars
	bcc:+
		ldx lfsr_tap+channel*5+channel_vars
		stx volume+channel*5+channel_vars
	:
	alr #%00111100
	lsr
	adc identity_table, y
	tay
	jmp (waveforms+chan3)


LFSR_chan2:
; Generate LFSR
	; 16,22 - 24,34 cycles (assumes zeropage)
	; 36 bytes
	dec counter+channel*5+channel_vars
	bne:++
		; reset counter
		lda divider+channel*5+channel_vars
		sta counter+channel*5+channel_vars
		; galois lfsr
		lda lfsr_tap+channel*5+channel_vars
		sre lfsr+channel*5+channel_vars			;lsr var, then eor var
		bcs:+
			sta lfsr+channel*5+channel_vars
			tya
			adc volume+channel*5+channel_vars
			tay
		:
		jmp (waveforms+chan3)	;TODO: copy this elsewhere
	:
	lda lfsr+channel*5+channel_vars
	and #%00000001
	bne:+
		tya
		adc volume+channel*5+channel_vars
		tay
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
	asl divider+channel*5+channel_vars
	bcs @read_high
;read_low:
		lda (lfsr+channel*5+channel_vars, X)
		and #%00001111
		inc lfsr+channel*5+channel_vars
		jmp @output
@read_high:
		inc divider+channel*5+channel_vars
		lda (lfsr+channel*5+channel_vars, X)
		lsr
		lsr
		lsr
		alr #$FE		;lsr, then clc
@output:
	adc identity_table, y
	tay
	jmp (waveforms+chan3)


;;; Channel 3
channel .set 2

Pulse_chan3:
; Generate Pulse Wave, optimized w/ noise vars (divider, counter, volume, lfsr, lfsr_tap)
	; lfsr stores state as a shift register. lfsr_tap holds high divider.
	; 14?,20? - 32?,34 cycles (assumes zeropage)
	; 18 bytes
	dec counter+channel*5+channel_vars
	bne:++
		;update state
		asl lfsr+channel*5+channel_vars
		;if state was low
		bcs:+
			lda divider+channel*5+channel_vars
			jmp @reset_end
		:
		;if state was high
			clc
			inc lfsr+channel*5+channel_vars
			lda lfsr_tap+channel*5+channel_vars
@reset_end:
		sta counter+channel*5+channel_vars
	:
	bit lfsr+channel*5+channel_vars
	; if plus, add volume to output
	bmi:+
		tya
		adc volume+channel*5+channel_vars
		tay
	:
	jmp (waveforms+chan4)

Linear_chan3:
; Generate Linear Wave (Triangle/Sawtooth)
	; lfsr_tap holds falling value (-), while lfsr hold rising value (+).
	; period ~=~ ciel(divider / lfsr) + ciel(divider / lfsr_tap)
	; setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
	; 28 - 33,35 (assumes zeropage)
	; 29 bytes
	lda counter+channel*5+channel_vars
	adc volume+channel*5+channel_vars
	bpl:+
		ldx lfsr+channel*5+channel_vars
		stx volume+channel*5+channel_vars
		lda #8
	:
	sta counter+channel*5+channel_vars
	cmp divider+channel*5+channel_vars
	bcc:+
		ldx lfsr_tap+channel*5+channel_vars
		stx volume+channel*5+channel_vars
	:
	alr #%00111100
	lsr
	adc identity_table, y
	tay
	jmp (waveforms+chan4)


LFSR_chan3:
; Generate LFSR
	; 16,22 - 24,34 cycles (assumes zeropage)
	; 36 bytes
	dec counter+channel*5+channel_vars
	bne:++
		; reset counter
		lda divider+channel*5+channel_vars
		sta counter+channel*5+channel_vars
		; galois lfsr
		lda lfsr_tap+channel*5+channel_vars
		sre lfsr+channel*5+channel_vars			;lsr var, then eor var
		bcs:+
			sta lfsr+channel*5+channel_vars
			tya
			adc volume+channel*5+channel_vars
			tay
		:
		jmp (waveforms+chan4)	;TODO: copy this elsewhere
	:
	lda lfsr+channel*5+channel_vars
	and #%00000001
	bne:+
		tya
		adc volume+channel*5+channel_vars
		tay
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
	asl divider+channel*5+channel_vars
	bcs @read_high
;read_low:
		lda (lfsr+channel*5+channel_vars, X)
		and #%00001111
		inc lfsr+channel*5+channel_vars
		jmp @output
@read_high:
		inc divider+channel*5+channel_vars
		lda (lfsr+channel*5+channel_vars, X)
		lsr
		lsr
		lsr
		alr #$FE		;lsr, then clc
@output:
	adc identity_table, y
	tay
	jmp (waveforms+chan4)


;;; Channel 4
channel .set 3

Pulse_chan4:
; Generate Pulse Wave, optimized w/ noise vars (divider, counter, volume, lfsr, lfsr_tap)
	; lfsr stores state as a shift register. lfsr_tap holds high divider.
	; 14?,20? - 32?,34 cycles (assumes zeropage)
	; 18? bytes
	dec counter+channel*5+channel_vars
	bne:++
		;update state
		asl lfsr+channel*5+channel_vars
		;if state was low
		bcs:+
			lda divider+channel*5+channel_vars
			jmp @reset_end
		:
		;if state was high
			clc
			inc lfsr+channel*5+channel_vars
			lda lfsr_tap+channel*5+channel_vars
@reset_end:
		sta counter+channel*5+channel_vars
	:
	tya
	bit lfsr+channel*5+channel_vars
	; if plus, add volume to output
	bmi:+
		adc volume+channel*5+channel_vars
		;tay
	:
	jmp Output_to_DPCM

Linear_chan4:
; Generate Linear Wave (Triangle/Sawtooth)
	; lfsr_tap holds falling value (-), while lfsr hold rising value (+).
	; period ~=~ ciel(divider / lfsr) + ciel(divider / lfsr_tap)
	; setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
	; 28 - 33,35 (assumes zeropage)
	; 29 bytes
	lda counter+channel*5+channel_vars
	adc volume+channel*5+channel_vars
	bpl:+
		ldx lfsr+channel*5+channel_vars
		stx volume+channel*5+channel_vars
		lda #8
	:
	sta counter+channel*5+channel_vars
	cmp divider+channel*5+channel_vars
	bcc:+
		ldx lfsr_tap+channel*5+channel_vars
		stx volume+channel*5+channel_vars
	:
	alr #%00111100
	lsr
	adc identity_table, y
	;tay
	jmp Output_to_DPCM


LFSR_chan4:
; Generate LFSR
	; 16?,22? - 24,34 cycles (assumes zeropage)
	; 36? bytes
	dec counter+channel*5+channel_vars
	bne:++
		; reset counter
		lda divider+channel*5+channel_vars
		sta counter+channel*5+channel_vars
		; galois lfsr
		lda lfsr_tap+channel*5+channel_vars
		sre lfsr+channel*5+channel_vars			;lsr var, then eor var
		bcs:+
			sta lfsr+channel*5+channel_vars
			tya
			adc volume+channel*5+channel_vars
			tay
		:
		tya
		jmp Output_to_DPCM
	:
	lda lfsr+channel*5+channel_vars
	and #%00000001
		tya
	bne:+
		adc volume+channel*5+channel_vars
		;tay
	:
	;tya
	jmp Output_to_DPCM
	

Sample_chan4:
; Generate Page Sample
	;samples are raw 4-bit, the sound engine must also manually end playback either with a new waveform or a blank sample
	; divider holds state. counter holds low byte of sample address, while volume holds the high byte
	; sample loops through a full page of data (512 samples)
	; 31? - 33 cycles (assuming zeropage)
	; 22? bytes
	ldx #0
	asl divider+channel*5+channel_vars
	bcs @read_high
;read_low:
		lda (lfsr+channel*5+channel_vars, X)
		and #%00001111
		inc lfsr+channel*5+channel_vars
		jmp @output
@read_high:
		inc divider+channel*5+channel_vars
		lda (lfsr+channel*5+channel_vars, X)
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
	
	;sta DMC_output
	; A holds output after returning
	
	;for testing
	;bit $2002

	; 6 cycles
	rts
	
	
.segment "RODATA"
Kick_sample:
	.byte $88,$7B,$51,$8C,$FF,$C5,$00,$00,$03,$8C,$FF,$FF,$FF,$EA,$62,$00
	.byte $00,$01,$37,$AE,$FF,$FF,$FF,$FD,$B9,$74,$21,$00,$00,$00,$02,$69
	.byte $BD,$FF,$FF,$FF,$FF,$FE,$CA,$85,$32,$00,$00,$00,$00,$00,$24,$69
	.byte $CE,$FF,$FF,$FF,$FF,$ED,$CB,$A9,$87,$64,$32,$10,$00,$00,$00,$00
	.byte $00,$00,$00,$12,$34,$57,$89,$AC,$DE,$EF,$FF,$FF,$FF,$FF,$FF,$FF
	.byte $FF,$FF,$FE,$DD,$CB,$A9,$98,$76,$65,$43,$32,$11,$11,$10,$00,$00
	.byte $00,$00,$11,$11,$12,$23,$45,$67,$89,$AB,$CD,$DE,$EE,$EE,$EE,$EE
	.byte $EE,$EE,$EE,$DD,$CC,$BA,$A9,$88,$77,$65,$54,$44,$33,$22,$22,$22
	.byte $22,$22,$22,$22,$22,$23,$33,$45,$56,$77,$89,$AA,$BB,$CC,$CC,$CC
	.byte $CD,$DD,$DD,$DC,$CC,$CC,$CC,$CB,$BA,$A9,$98,$87,$77,$66,$55,$55
	.byte $44,$44,$33,$33,$33,$33,$33,$44,$44,$44,$55,$55,$56,$66,$77,$77
	.byte $88,$88,$99,$99,$99,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$A9,$99,$99
	.byte $99,$99,$98,$88,$88,$87,$77,$77,$77,$77,$76,$66,$66,$66,$66,$66
	.byte $66,$66,$66,$66,$67,$77,$77,$77,$77,$77,$77,$78,$88,$88,$88,$88
	.byte $88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88,$88
	.byte $88,$87,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77

Snare_sample:
	.byte $67,$77,$77,$42,$05,$CF,$FF,$A0,$00,$07,$8C,$FF,$FF,$D1,$00,$00
	.byte $5C,$C7,$CF,$FC,$D9,$92,$97,$40,$27,$FD,$B9,$53,$99,$60,$00,$02
	.byte $5B,$88,$FF,$F8,$11,$15,$AB,$D8,$AE,$FF,$BC,$30,$27,$A4,$54,$97
	.byte $CA,$93,$37,$B6,$54,$31,$6A,$C6,$58,$97,$77,$43,$78,$AA,$7A,$A8
	.byte $BA,$B7,$26,$AB,$99,$B9,$BB,$B8,$46,$77,$69,$85,$57,$9A,$74,$46
	.byte $6A,$87,$35,$79,$78,$77,$57,$86,$54,$57,$AA,$AA,$77,$8A,$86,$77
	.byte $78,$9A,$97,$9A,$99,$87,$77,$8A,$87,$79,$97,$88,$77,$79,$76,$66
	.byte $77,$78,$76,$78,$97,$77,$87,$78,$76,$57,$77,$88,$88,$77,$87,$77
	.byte $87,$78,$88,$78,$98,$78,$87,$77,$77,$77,$88,$78,$88,$77,$87,$77
	.byte $88,$88,$88,$77,$87,$77,$88,$78,$88,$88,$88,$77,$77,$77,$78,$78
	.byte $88,$87,$88,$78,$88,$77,$87,$77,$78,$88,$88,$77,$77,$77,$88,$88
	.byte $88,$77,$77,$77,$77,$77,$88,$87,$77,$77,$88,$88,$78,$77,$77,$88
	.byte $88,$77,$77,$78,$88,$88,$88,$77,$77,$88,$88,$87,$78,$88,$77,$77
	.byte $78,$77,$77,$78,$88,$88,$78,$87,$77,$78,$78,$88,$88,$78,$88,$87
	.byte $77,$87,$77,$88,$78,$88,$87,$77,$78,$88,$88,$88,$88,$88,$77,$77
	.byte $77,$88,$88,$88,$88,$77,$77,$77,$77,$78,$88,$88,$87,$77,$77,$88
	
	
divide_by_16:
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
	.byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
	.byte $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
	.byte $06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06
	.byte $07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07
	.byte $08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08
	.byte $09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09,$09
	.byte $0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a
	.byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
	.byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c
	.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
	.byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
	.byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
	
	
; Some good LFSR taps
; seed = 1
; 11100011 = period of 12
; $E4 = period of 63 (roughly A#1 in A4=453, but sounds much higher)
; $E6 = period of 56?
; seed $0E, tap $E8, period of 5
; seed $08, tap $E8, period of 15
; seed $01, tap $EB, period of 17


; all of these might be wrong, actually. I don't really know

; seed = 00000001
; 11000111 = period of 12
; 00010001 = period of 12
; 00010101 = period of 14
; 01010001 = period of 14
; 11010111 = period of 17
; 11111111 = period of 9

;all below are wrong?
; 00000001 = period of 8
; 10100101 = period of 21
; 10101101 = period of 35
; 11101101 = period of 30
; 00000101 = period of 30
; 10111011 - period of 15
; 11101111 - period of 20

; seed = 10000000
; 10100000 = period of 7
; 11000000 = period of 3
; 10010000 = period of 15
; 11100000 = period of 4
; 11110000 = period of 5
; 10110000 = period of 6