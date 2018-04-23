;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================
;;;
;;; Pascal driver

        .setcpu "6502"

        .include "apple2.inc"
        .include "../inc/apple2.inc"
        .include "../mgtk/mgtk.inc"

        .import MGTK__interrupt_handler__body


        .segment "MGTK_DISPATCH"

        ptr := 4
        save_ret := 6

.proc mgtk_pascal_driver


pascal_entry:
        pla
        sta     save_ret
        pla
        sta     save_ret+1
        txa
        asl     a
        tay
        lda     func_table+1,y
        pha
        lda     func_table,y
        pha
        rts

no_op:
exit_3: ldx     #3
        bne     exit_x

exit_0: ldx     #0
exit_x: lda     save_ret+1
        pha
        lda     save_ret
        pha
        rts


func_table:
        .addr   read_op-1
        .addr   write_op-1
        .addr   unitclear_op-1
        .addr   no_op-1
        .addr   status_op-1


;;; Pascal driver entry point: UNIT CLEAR

unitclear_op:
        lda     init_flag
        beq     do_init
        lda     a:slot_page
        bne     :+

        ldx     #9
        jmp     exit_x

:       php
        sei
        lda     #$00
        jsr     call_setmouse
        plp
        jmp     exit_0_jmp

do_init:
        inc     a:init_flag
        jsr     find_slot
        bcc     :+
        jmp     exit_x

:       lda     $FFFE
        sta     a:save_irq
        lda     $FFFF
        sta     a:save_irq+1

        lda     a:irq_entry_addr
        sta     $FFFE
        lda     a:irq_entry_addr+1
        sta     $FFFF

        ldx     a:slot_page
        ldy     #0

        stx     a:slot_setmouse+1
        lda     a:slot_setmouse
        sta     ptr
        lda     a:slot_setmouse+1
        sta     ptr+1

        lda     (ptr),y
        sta     a:slot_setmouse

        stx     a:slot_servemouse+1
        lda     a:slot_servemouse
        sta     ptr
        lda     a:slot_servemouse+1
        sta     ptr+1

        lda     (ptr),y
        sta     a:slot_servemouse

exit_0_jmp:
        jmp     exit_0


find_slot:
        ldx     #7
        lda     #$00
        sta     4
        lda     #$C8
        sta     ptr+1

scanloop:
        dec     ptr+1
        dex
        bmi     notfound

        ldy     #$0C
        lda     (ptr),y
        cmp     #$20
        bne     scanloop

        ldy     #$FB
        lda     (ptr),y
L00AD:  cmp     #$D6
        bne     scanloop

        lda     ptr+1
        sta     a:slot_page
        asl     a
        asl     a
        asl     a
        asl     a
        sta     a:slot_x16
        clc
        rts

notfound:
        ldx     #9
        sec
        rts


irq_entry_addr:
        .addr   irq_entry


slot_page:
        .byte   0
slot_x16:
        .byte   0

slot_setmouse:
        .addr   SETMOUSE
slot_servemouse:
        .addr   SERVEMOUSE

mgtk_irq_handler_addr:
        .addr   MGTK__interrupt_handler__body

        .addr   mgtk_irq_handler_jmp-1      ; unused ???

save_irq:
        .addr   0

init_flag:
        .byte   0


;;; Pascal driver entry point: UNIT READ

read_op:
        pla
        pla
        pla
        pla
        pla
        pla
        pla
        pla
        pla
        pla
        ldx     #3
        jmp     exit_x


;;; Pascal driver entry point: UNIT WRITE

write_op:
        pla
        pla
        pla
        sta     mli_func
        pla
        pla
        sta     mli_param
        pla
        sta     mli_param+1
        pla
        pla
        pla
        pla

        lda     mli_func
        cmp     #$FF              ; ToolkitAddress
        bne     do_call

        lda     mli_param
        sta     ptr
        lda     mli_param+1
        sta     ptr+1

        ldy     #0
        lda     mli_addr
        sta     (ptr),y
        iny
        lda     mli_addr+1
        sta     (ptr),y
        jmp     exit_0

mli_addr: .addr MGTK::MLI


do_call:
        lda     #$FF
        sta     PAGE2OFF
        sta     SET80COL

        jsr     MGTK::MLI
mli_func:
        .byte 0
mli_param:
        .addr 0
        tax
        lda     #$FF
        jmp     exit_x


;;; Pascal driver entry point: UNIT STATUS

status_op:
        pla
        pla
        pla
        pla
        ldx     #3
        jmp     exit_x

call_setmouse:
        ldx     a:slot_page
        stx     $07F8
        ldy     a:slot_x16
        jmp     (slot_setmouse)

call_servemouse:
        ldx     a:slot_page
        ldy     a:slot_x16
        jmp     (slot_servemouse)

irq_entry:
        jsr     call_servemouse
        bcc     :+
        clc
        rti

        jmp     save_irq_jmp

:       jsr     mgtk_irq_handler_jmp
        rti

        rts

mgtk_irq_handler_jmp:
        jmp     (mgtk_irq_handler_addr)

save_irq_jmp:
        jmp     (save_irq)


.endproc
