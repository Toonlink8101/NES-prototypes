.segment "CODE"

Audio_handler:
	;inc frame count
	lda frame_count
	anc #%00111111		;clears carry
	adc #1
	sta frame_count
	tay
	
	; pattern count
	lda pattern_count
	
	cpy #1
	bne:+
		anc #%00000111
		adc #1
		sta pattern_count
	:
	
	cmp #5
	bcc:+
		jmp pattern1
	:
	
	; channel 3 PWM
	;channel .set 2*5
	;dec channel_vars+channel+divider
	;bne:+
	;	lda #12
	;	sta channel_vars+channel+divider
	;:
	;
	;lda #12
	;sec
	;sbc channel_vars+channel+divider
	;sta channel_vars+channel+lfsr_tap
	
	;channel 1 detune
	channel .set 0*5
	inc channel_vars+channel+counter
	;dec also works, I think
	
pattern0:
	; music "pattern" data
	cpy #$01
	bne:+
		lda #<Pulse_chan1
		ldx #>Pulse_chan1
		sta waveforms+0
		stx waveforms+1
		
		channel .set 0*5
		lda #10
		sta channel_vars+channel+divider
		lda #0
		sta channel_vars+channel+counter
		lda #$02
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #10
		sta channel_vars+channel+lfsr_tap
		
		
		lda #<Pulse_chan2
		ldx #>Pulse_chan2
		sta waveforms+2
		stx waveforms+3
		
		channel .set 1*5
		lda #10
		sta channel_vars+channel+divider
		lda #1
		sta channel_vars+channel+counter
		lda #$03
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #10
		sta channel_vars+channel+lfsr_tap
		
		
		lda #<Pulse_chan3
		ldx #>Pulse_chan3
		sta waveforms+4
		stx waveforms+5
		
		channel .set 2*5
		lda #6
		sta channel_vars+channel+divider
		lda #2
		sta channel_vars+channel+counter
		lda #$02
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #6
		sta channel_vars+channel+lfsr_tap
		
		
		lda #<Sample_chan4
		ldx #>Sample_chan4
		sta waveforms+6
		stx waveforms+7
		
		channel .set 3*5
		lda #$AA
		sta channel_vars+channel+divider
		lda #<Kick_sample
		sta channel_vars+channel+lfsr
		lda #>Kick_sample
		sta channel_vars+channel+lfsr_tap
	:
	
	cpy #$07
	bne:+
		lda #<Pulse_chan4
		ldx #>Pulse_chan4
		sta waveforms+6
		stx waveforms+7
		
		channel .set 3*5
		lda #15
		sta channel_vars+channel+divider
		lda #0
		sta channel_vars+channel+counter
		lda #$04
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #15
		sta channel_vars+channel+lfsr_tap
	:

	
	cpy #$11
	bne:+
		lda #<Sample_chan4
		ldx #>Sample_chan4
		sta waveforms+6
		stx waveforms+7
		
		channel .set 3*5
		lda #$AA
		sta channel_vars+channel+divider
		lda #<Kick_sample
		sta channel_vars+channel+lfsr
		lda #>Kick_sample
		sta channel_vars+channel+lfsr_tap
	:
	
	cpy #$13
	bne:+
		lda #<Pulse_chan4
		ldx #>Pulse_chan4
		sta waveforms+6
		stx waveforms+7
		
		channel .set 3*5
		lda #15
		sta channel_vars+channel+divider
		lda #0
		sta channel_vars+channel+counter
		lda #$04
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #15
		sta channel_vars+channel+lfsr_tap
	:
	
	cpy #$21
	bne:+
		lda #<Sample_chan4
		ldx #>Sample_chan4
		sta waveforms+6
		stx waveforms+7
		
		channel .set 3*5
		lda #$AA
		sta channel_vars+channel+divider
		lda #<Snare_sample
		sta channel_vars+channel+lfsr
		lda #>Snare_sample
		sta channel_vars+channel+lfsr_tap
	:
	
	cpy #$25
	bne:+
		lda #<Pulse_chan4
		ldx #>Pulse_chan4
		sta waveforms+6
		stx waveforms+7
		
		channel .set 3*5
		lda #15
		sta channel_vars+channel+divider
		lda #0
		sta channel_vars+channel+counter
		lda #$04
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #15
		sta channel_vars+channel+lfsr_tap
	:
	
	cpy #$31
	
	cpy #$37
	
jmp end_pattern

pattern1:	
	cpy #$01
	bne:+
		lda #<Pulse_chan1
		ldx #>Pulse_chan1
		sta waveforms+0
		stx waveforms+1
		
		channel .set 0*5
		lda #4
		sta channel_vars+channel+divider
		lda #0
		sta channel_vars+channel+counter
		lda #$02
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #4
		sta channel_vars+channel+lfsr_tap
		
		
		lda #<Linear_chan2
		ldx #>Linear_chan2
		sta waveforms+2
		stx waveforms+3
		
		channel .set 1*5
		lda #60
		sta channel_vars+channel+divider
		lda #2
		sta channel_vars+channel+counter
		lda #$02
		sta channel_vars+channel+volume
		lda #5
		sta channel_vars+channel+lfsr
		lda #(5 ^$FF)+1	;negates value
		sta channel_vars+channel+lfsr_tap
		
		
		lda #<Linear_chan3
		ldx #>Linear_chan3
		sta waveforms+4
		stx waveforms+5
		
		channel .set 2*5
		lda #20
		sta channel_vars+channel+divider
		lda #02
		sta channel_vars+channel+counter
		lda #$02
		sta channel_vars+channel+volume
		lda #4
		sta channel_vars+channel+lfsr
		lda #(4 ^$FF)+1	;negates value
		sta channel_vars+channel+lfsr_tap
		
		
		lda #<Sample_chan4
		ldx #>Sample_chan4
		sta waveforms+6
		stx waveforms+7
		
		channel .set 3*5
		lda #$AA
		sta channel_vars+channel+divider
		lda #<Kick_sample
		sta channel_vars+channel+lfsr
		lda #>Kick_sample
		sta channel_vars+channel+lfsr_tap
	:
	
	cpy #$07
	bne:+
		lda #<Pulse_chan4
		ldx #>Pulse_chan4
		sta waveforms+6
		stx waveforms+7
		
		channel .set 3*5
		lda #15
		sta channel_vars+channel+divider
		lda #3
		sta channel_vars+channel+counter
		lda #$0;5
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #15
		sta channel_vars+channel+lfsr_tap
	:

	
	cpy #$11
	bne:+
		lda #<Sample_chan4
		ldx #>Sample_chan4
		sta waveforms+6
		stx waveforms+7
		
		channel .set 3*5
		lda #$AA
		sta channel_vars+channel+divider
		lda #<Kick_sample
		sta channel_vars+channel+lfsr
		lda #>Kick_sample
		sta channel_vars+channel+lfsr_tap
	:
	
	cpy #$17
	bne:+
		lda #<Pulse_chan4
		ldx #>Pulse_chan4
		sta waveforms+6
		stx waveforms+7
		
		channel .set 3*5
		lda #15
		sta channel_vars+channel+divider
		lda #3
		sta channel_vars+channel+counter
		lda #$0;5
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #15
		sta channel_vars+channel+lfsr_tap
	:
	
	cpy #$21
	
	cpy #$27
	bne:+
		lda #<Pulse_chan1
		ldx #>Pulse_chan1
		sta waveforms+0
		stx waveforms+1
		
		channel .set 0*5
		lda #4
		sta channel_vars+channel+divider
		lda #0
		sta channel_vars+channel+counter
		lda #$0
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #4
		sta channel_vars+channel+lfsr_tap
		
		
		lda #<Pulse_chan2
		ldx #>Pulse_chan2
		sta waveforms+2
		stx waveforms+3
		
		channel .set 1*5
		lda #10
		sta channel_vars+channel+divider
		lda #1
		sta channel_vars+channel+counter
		lda #$0
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #10
		sta channel_vars+channel+lfsr_tap
		
		
		lda #<Pulse_chan3
		ldx #>Pulse_chan3
		sta waveforms+4
		stx waveforms+5
		
		channel .set 2*5
		lda #6
		sta channel_vars+channel+divider
		lda #2
		sta channel_vars+channel+counter
		lda #$0
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #6
		sta channel_vars+channel+lfsr_tap
	:
	
	cpy #$31
	bne:+
		lda #<Sample_chan4
		ldx #>Sample_chan4
		sta waveforms+6
		stx waveforms+7
		
		channel .set 3*5
		lda #$AA
		sta channel_vars+channel+divider
		lda #<Snare_sample
		sta channel_vars+channel+lfsr
		lda #>Snare_sample
		sta channel_vars+channel+lfsr_tap
	:
	
	cpy #$37
	bne:+
		lda #<Pulse_chan4
		ldx #>Pulse_chan4
		sta waveforms+6
		stx waveforms+7
		
		channel .set 3*5
		lda #15
		sta channel_vars+channel+divider
		lda #0
		sta channel_vars+channel+counter
		lda #$0;5
		sta channel_vars+channel+volume
		lda #$AA
		sta channel_vars+channel+lfsr
		lda #15
		sta channel_vars+channel+lfsr_tap
	:
	
	
	
end_pattern:

	rts