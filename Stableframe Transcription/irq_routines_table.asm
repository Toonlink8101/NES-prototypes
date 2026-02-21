; python gen_irq_routines_table.py > src/irq_routines_table.asm

        .align 256
irq_routines_table:
        .word irq_routine_vblank_start
            .byte DMCFREQ_IRQ_RATE428
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE428
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE106
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE54
        .word irq_routine_two_step
            .byte DMCFREQ_IRQ_RATE190, DMCFREQ_IRQ_RATE54

        .word irq_routine_row_light - 4
            .byte DMCFREQ_IRQ_RATE84
        .word irq_routine_row_dark - 0
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_light - 4
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_dark - 6
            .byte DMCFREQ_IRQ_RATE84
        .word irq_routine_row_light - 2
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_dark - 4
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_light - 8
            .byte DMCFREQ_IRQ_RATE84
        .word irq_routine_row_dark - 2
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_light - 6
            .byte DMCFREQ_IRQ_RATE84
        .word irq_routine_row_dark - 0
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_light - 4
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_dark - 8
            .byte DMCFREQ_IRQ_RATE84
        .word irq_routine_row_light - 2
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_dark - 6
            .byte DMCFREQ_IRQ_RATE84
        .word irq_routine_row_light - 0
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_dark - 4
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_light - 6
            .byte DMCFREQ_IRQ_RATE84
        .word irq_routine_row_dark - 2
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_light - 4
            .byte DMCFREQ_IRQ_RATE84
        .word irq_routine_row_dark - 0
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_light - 4
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_dark - 6
            .byte DMCFREQ_IRQ_RATE84
        .word irq_routine_row_light - 2
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_dark - 4
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_light - 8
            .byte DMCFREQ_IRQ_RATE84
        .word irq_routine_row_dark - 2
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_light - 6
            .byte DMCFREQ_IRQ_RATE84
        .word irq_routine_row_dark - 0
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_light - 4
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_dark - 8
            .byte DMCFREQ_IRQ_RATE84
        .word irq_routine_row_light - 2
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_row_dark - 6
            .byte DMCFREQ_IRQ_RATE84

        .word irq_routine_align_start
            .byte DMCFREQ_IRQ_RATE54

irq_routines_table_align_0:
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE380
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE254
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE72
        .word irq_routine_two_step_align
            .byte DMCFREQ_IRQ_RATE72, DMCFREQ_IRQ_RATE54, <irq_routines_table_align_3
        ; cpu cycles = 29782    offset = -1.5

irq_routines_table_align_1:
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE428
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE226
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE54
        .word irq_routine_one_step_align
            .byte DMCFREQ_IRQ_RATE54, <irq_routines_table_align_0
        ; cpu cycles = 29780    offset = 0.5

irq_routines_table_align_2:
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE428
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE226
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE54
        .word irq_routine_one_step_align
            .byte DMCFREQ_IRQ_RATE54, <irq_routines_table_align_1
        ; cpu cycles = 29780    offset = 0.5

irq_routines_table_align_3:
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE428
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE226
        .word irq_routine_one_step
            .byte DMCFREQ_IRQ_RATE54
        .word irq_routine_one_step_align
            .byte DMCFREQ_IRQ_RATE54, <irq_routines_table_align_2
        ; cpu cycles = 29780    offset = 0.5