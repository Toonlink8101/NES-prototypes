;;;;
; New idea for DPCM-driven AV
;;;;

; Instead of having DMC IRQ drive everything, it seems that having 
; it drive only audio/video effects during the frame would make more 
; sense. While capable of producing a stable cycle of interrupts, it
; seems that haing it interrupt so often makes that kind of stability 
; difficult. Even with hours of testing values, the closest I could
; replicate drifts slightly after several frames. It seems that using
; NMI as a syncing method would be more consistant and reliable, even
; if that interrupt itself also has inconsistancies. The frequencies
; found in the previous method should still be a good starting point
; for the syncing needed for the frame. Only a pre-determined number
; of IRQs will be triggered in a frame, with a counter keeping track.
; This counter is reset with the NMI routine, which also triggers the 
; first DMC sample. This loops throughout the frame, ending when the
; counter reaches zero, where then the IRQs stop until NMI restarts them.

; Aside from implementing a proper NMI routine, this only changes the
; IRQ routine from always looping to looping a set number of times.
; Adjusting the timing of the IRQs might prove difficult, much like
; time, but at least NMI negates the need for synchronousy.

; Given that each frame only has 262 scanlines and that OAM DMA will
; run each frame, only 64 IRQs will occur each frame. This limits the
; amount of data needed for each effect to multiples of 64 at worst.
; Even a lookup table for sine-wave functions could be more easily
; compressed.

; Rough plan of interrupts:
; NMI start
	; update DMC $4011 with special buffer
	; run OAM DMA
	; read controllers?
	; jump to IRQ routine
		; update DMC $4011 with usual buffer
		; interate audio
; vblank thing occasionally interrupted by irq
; after vblank, irq interrupts regularly for audio, sometimes with visual effects
; at the last irq before nmi, the irq interates twice: 
	; once for the special buffer, then again for the usual buffer.
	
; irq trick!???!?!
; for NMI things






;Overworld scrolling

;updating attributes

lda old_attr
and %00110011	; zero out right half
eor new_attr	; should already have left half zeroed
sta attr




;16 bit lsr
lsr var_high
ror var_low

