;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"



        .import DRAW_LINE_ABS_IMPL_do_draw_line
        .import load_poly
        .import next_poly
        .import ora_2_param_bytes
        .importzp poly_oper
        .import poly_oper_paint
        .import poly_oper_test
        .importzp vertices_count


;;; ============================================================
;;; FramePoly

.proc FramePolyImpl
        lda     #0
        sta     poly_oper
        jsr     ora_2_param_bytes

        ptr := $B7
        draw_line_params := $92

poly_loop:
        copy16  params_addr, ptr

        lda     vertices_count             ; ORAd param bytes
        sta     $B6
        ldx     #0
        jsr     load_poly
        bcc     next

        lda     $B3
        sta     $B5             ; loop counter

        ;; Loop for drawing
        ldy     #0
loop:   dec     $B5
        beq     endloop
        sty     $B9

        ldx     #0
:       lda     (ptr),y
        sta     draw_line_params,x
        iny
        inx
        cpx     #8
        bne     :-
        jsr     DRAW_LINE_ABS_IMPL_do_draw_line

        lda     $B9
        clc
        adc     #4
        tay
        bne     loop

endloop:
        ;; Draw from last point back to start
        ldx     #0
:       lda     (ptr),y
        sta     draw_line_params,x
        iny
        inx
        cpx     #4
        bne     :-
        ldy     #3
:       lda     (ptr),y
        sta     draw_line_params+4,y
        sta     current_penloc,y
        dey
        bpl     :-
        jsr     DRAW_LINE_ABS_IMPL_do_draw_line

        ;; Handle multiple segments, e.g. when drawing outlines for multi icons?

next:   ldx     #1
:       lda     ptr,x
        sta     $80,x
        lda     $B5,x
        sta     $B3,x
        dex
        bpl     :-

        jsr     next_poly           ; Advance to next polygon in list
        bmi     poly_loop
        rts
.endproc

        .include "framepoly-exp.inc"

