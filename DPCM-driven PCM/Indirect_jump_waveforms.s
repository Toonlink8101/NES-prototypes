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
	; lfsr_tap holds falling value (-), while lfsr hold rising value (+).
	; period ~=~ ciel(divider / lfsr) + ciel(divider / lfsr_tap)
	; setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
	; 28 - 33,35 (assumes zeropage)
	; 29 bytes
	lda counter+channel
	adc volume+channel
	bpl:+
		ldx lfsr+channel
		stx volume+channel
		lda #8
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
; Generate LFSR
	; 16,20 - 24,32 cycles (assumes zeropage)
	; 36? bytes
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
			lda volume+channel
			tay
		:
		jmp (waveforms+chan4)	;TODO: copy this elsewhere
	:
	lda lfsr+channel
	and #%00000001
	bne:+
		;tya
		;adc volume+channel
		lda volume+channel
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
	; lfsr_tap holds falling value (-), while lfsr hold rising value (+).
	; period ~=~ ciel(divider / lfsr) + ciel(divider / lfsr_tap)
	; setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
	; 28 - 33,35 (assumes zeropage)
	; 29 bytes
	lda counter+channel
	adc volume+channel
	bpl:+
		ldx lfsr+channel
		stx volume+channel
		lda #8
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
; Generate LFSR
	; 16,22 - 24,34 cycles (assumes zeropage)
	; 36 bytes
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
		jmp (waveforms+chan3)	;TODO: copy this elsewhere
	:
	lda lfsr+channel
	and #%00000001
	bne:+
		tya
		adc volume+channel
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
	; lfsr_tap holds falling value (-), while lfsr hold rising value (+).
	; period ~=~ ciel(divider / lfsr) + ciel(divider / lfsr_tap)
	; setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
	; 28 - 33,35 (assumes zeropage)
	; 29 bytes
	lda counter+channel
	adc volume+channel
	bpl:+
		ldx lfsr+channel
		stx volume+channel
		lda #8
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
; Generate LFSR
	; 16,22 - 24,34 cycles (assumes zeropage)
	; 36 bytes
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
		jmp (waveforms+chan4)	;TODO: copy this elsewhere
	:
	lda lfsr+channel
	and #%00000001
	bne:+
		tya
		adc volume+channel
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
	; lfsr_tap holds falling value (-), while lfsr hold rising value (+).
	; period ~=~ ciel(divider / lfsr) + ciel(divider / lfsr_tap)
	; setting lfsr == divider and lfsr_tap == -1 results in a sawtooth
	; 28 - 33,35 (assumes zeropage)
	; 29 bytes
	lda counter+channel
	adc volume+channel
	bpl:+
		ldx lfsr+channel
		stx volume+channel
		lda #8
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
; Generate LFSR
	; 16?,22? - 24,34 cycles (assumes zeropage)
	; 36? bytes
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
		jmp Output_to_DPCM
	:
	lda lfsr+channel
	and #%00000001
	bne:+
		tya
		adc volume+channel
		jmp Output_to_DPCM
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