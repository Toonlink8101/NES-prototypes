.globalzp preserve_A, preserve_X, preserve_Y, DMC_output

.segment "BSS"
	waveforms: .res 8		;2 bytes per pointer

.segment "ZEROPAGE"
	channel_vars: .res 20
	chan_output: .res 4

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
	
	
;Generate Pulse Wave, w/ output var
	; trashes A and X
	; lfsr stores state as a shift register. Whenever bit7 is set, volume will be aded to Y. $AA or $55 will oscillate normally
	; divider and lfsr_tap hold the duration of the silent and outputed segments respectively.
	; counter is the time left until the state (lfsr) is updated, whereupon it is reset to either divider or lfsr_tap. Can be treated like phase
.macro PulseOutput channel
	; 8 - 29,33 cycles
	; 18 bytes?
	dec counter+channel*5+channel_vars			;5
	bne:++										;2,3 if taken
		;update state
		asl lfsr+channel*5+channel_vars			;5
		;if state was low
		bcs:+									;2,3 if taken
			lda divider+channel*5+channel_vars	;3
			ldx volume+channel*5+channel_vars	;3
			jmp @reset_end						;3
		:
		;if state was high
			clc									;2
			inc lfsr+channel*5+channel_vars		;5
			lda lfsr_tap+channel*5+channel_vars	;3
			ldx #0								;2
@reset_end:
		sta counter+channel*5+channel_vars		;3
		stx chan_output+channel					;3
	:
.endmacro


;Generate Linear Wave, better volume, output var
	;trashes X, leaves output in A
	; Generates triangles, saws, and anything inbetween
	; counter holds the output value
	; volume is added to counter each iteration
	; divider is the "ceiling" for counter. Once counter passes this value, counter is set to lfsr_tap
	; lfsr_tap holds the decreasing value (-). It's loaded into volume when counter passes divider
	; lfsr hold the increasing value (+). It's loaded into volume when counter underflows
	; The period of this generator is roughly as follows: Period = ceil(divider / lfsr) + ciel(divider / lfsr_tap)
	; Setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
.macro LinearOutput channel
	; 25 - 31,32 cycles
	; ?? bytes
	lda counter+channel*5+channel_vars		;3
	adc volume+channel*5+channel_vars		;3
	bpl:+									;2,3 if taken
		ldx lfsr+channel*5+channel_vars		;3
		stx volume+channel*5+channel_vars	;3
		lda #8								;2
	:
	sta counter+channel*5+channel_vars		;3
	cmp divider+channel*5+channel_vars		;3
	bcc:+									;2,3 if taken
		ldx lfsr_tap+channel*5+channel_vars	;3
		stx volume+channel*5+channel_vars	;3
	:
	alr #%01111100							;2
	lsr										;2
	sta chan_output+channel					;3
.endmacro


;Generate LFSR Wave w/ output var
	; trashes A, adds output to Y
	;When counter hits zero, the LFSR state is iterated
	; divider is the value counter is reset to
	; naturally, lfsr_tap holds the tap for the LFSR, and lfsr holds the current lfsr state
.macro LFSRwOuput channel
	; 8 - 29,35 cycles
	; ?? bytes
	dec counter+channel*5+channel_vars			;5
	bne:++										;2,3 if taken
		; reset counter
		lda divider+channel*5+channel_vars		;3
		sta counter+channel*5+channel_vars		;3
		; galois lfsr
		lda lfsr_tap+channel*5+channel_vars		;3
		sre lfsr+channel*5+channel_vars			;5				;lsr var, then eor var
		bcs:+									;2,3 if taken
			sta lfsr+channel*5+channel_vars		;3
			lda volume+channel*5+channel_vars	;3
			sta chan_output+channel				;3
			jmp:++								;3
		:
		lda #0									;2
		sta chan_output+channel					;3
	:
.endmacro


;Generate Sample Optimized, w/ output var
	; trashes A, X, and Y
	; samples are 4-bit, with two stored in each byte
	; Playback will endlessly loop through a full page (256 bytes/512 samples) until manually stopped by changing the waveform
	; divider holds state as a shift register. When bit7 is clear, the low nibble is output. 
		; Otherwise, the high nibble is output and counter is incremented
		; Loading $55 into divider plays samples normally. $FF will skip every other sample. $11 will halve(?) playback speed
	; lfsr holds the low byte of the sample address. 
	; lfsr_tap holds the high byte of the sample address.
.macro SampleOutput channel
	; 24 - 31 cycles
	; ?? bytes
	ldy #0										;2
	asl divider+channel*5+channel_vars			;5
	bcs @read_low								;2, 3 if taken
;read_high:
		lax (lfsr+channel*5+channel_vars), Y	;5
		lda divide_by_16, X						;4
		jmp @output								;3
@read_low:
		lda (lfsr+channel*5+channel_vars), Y	;5
		anc #%00001111		;and #i, clc		;2
		inc lfsr+channel*5+channel_vars			;5
		inc divider+channel*5+channel_vars		;5
@output:
	sta chan_output+channel						;3
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
	jmp (waveforms+0*2)


; all waveforms for all channels
; use jmp (var) to go next channel waveform
	channel_count .set 0

Pulse_chan1:
	PulseOutput channel_count
	jmp (waveforms+channel_count*2)

Linear_chan1:
	LinearOutput channel_count
	jmp (waveforms+channel_count*2)

LFSR_chan1:
	LFSRwOutput channel_count
	jmp (waveforms+channel_count*2)

Sample_chan1:
	SampleOutput channel_count
	jmp (waveforms+channel_count*2)



channel_count .set 1

Pulse_chan2:
	PulseOutput channel_count
	jmp (waveforms+channel_count*2)

Linear_chan2:
	LinearOutput channel_count
	jmp (waveforms+channel_count*2)

LFSR_chan2:
	LFSRwOutput channel_count
	jmp (waveforms+channel_count*2)

Sample_chan2:
	SampleOutput channel_count
	jmp (waveforms+channel_count*2)



channel_count .set 2

Pulse_chan3:
	PulseOutput channel_count
	jmp (waveforms+channel_count*2)

Linear_chan3:
	LinearOutput channel_count
	jmp (waveforms+channel_count*2)

LFSR_chan3:
	LFSRwOutput channel_count
	jmp (waveforms+channel_count*2)

Sample_chan3:
	SampleOutput channel_count
	jmp (waveforms+channel_count*2)



channel_count .set 3

Pulse_chan4:
	PulseOutput channel_count
	jmp Sum_output

Linear_chan4:
	LinearOutput channel_count
	jmp Sum_output

LFSR_chan4:
	LFSRwOutput channel_count
	jmp Sum_output

Sample_chan4:
	SampleOutput channel_count
	jmp Sum_output


Sum_output:
	; 16 cycles
	lda chan_output
	clc
	adc chan_output+1
	adc chan_output+2
	adc chan_output+3
	asl
	
	;return
	rts