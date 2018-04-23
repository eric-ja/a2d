;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================
;;;
;;; Rectangle operations


        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        MGTK_IMPORT exit_with_a
        MGTK_IMPORT do_fill
        MGTK_IMPORT set_dest
        MGTK_IMPORT set_up_fill_mode


;;; ============================================================
;;; FrameRect

;;; 8 bytes of params, copied to $9F

frect_ctr:  .byte   0

.proc FrameRectImpl
        params  := $9F

        left    := params
        top     := params + 2
        right   := params + 4
        bottom  := params + 6

        ldy     #3
rloop:  ldx     #7
:       lda     left,x
        sta     left_masks_table,x
        dex
        bpl     :-
        ldx     rect_sides,y
        lda     left,x
        pha
        lda     $A0,x
        ldx     rect_coords,y
        sta     $93,x
        pla
        sta     left_masks_table,x
        sty     frect_ctr
        jsr     draw_line
        ldy     frect_ctr
        dey
        bpl     rloop
        ldx     #3
:       lda     left,x
        sta     current_penloc,x
        dex
        bpl     :-
.endproc
prts:   rts

rect_sides:
        .byte   0,2,4,6
rect_coords:
        .byte   4,6,0,2

.proc draw_line
        x2      := right

        lda     current_penwidth    ; Also: draw horizontal line $92 to $96 at $98
        sec
        sbc     #1
        cmp     #$FF
        beq     prts
        adc     x2
        sta     x2
        bcc     :+
        inc     x2+1

:       lda     current_penheight
        sec
        sbc     #1
        cmp     #$FF
        beq     prts
        adc     bottom
        sta     bottom
        bcc     PaintRectImpl
        inc     bottom+1
        ;; Fall through...
.endproc


;;; ============================================================
;;; PaintRect

;;; 8 bytes of params, copied to $92

.proc PaintRectImpl
        params := $92

        jsr     check_rect
do_paint:
        jsr     clip_rect
        bcc     prts
        jsr     set_up_fill_mode
        jsr     set_dest
        jmp     do_fill
.endproc


;;; ============================================================
;;; InRect

;;; 8 bytes of params, copied to $92

.proc InRectImpl
        params := $92

        jsr     check_rect
        ldax    current_penloc_x
        cpx     left+1
        bmi     fail
        bne     :+
        cmp     left
        bcc     fail

:       cpx     right+1
        bmi     :+
        bne     fail
        cmp     right
        bcc     :+
        bne     fail

:       ldax    current_penloc_y
        cpx     top+1
        bmi     fail
        bne     :+
        cmp     top
        bcc     fail

:       cpx     bottom+1
        bmi     :+
        bne     fail
        cmp     bottom
        bcc     :+
        bne     fail
:       exit_call MGTK::inrect_inside           ; success!

fail:   rts
.endproc

;;; ============================================================
;;; SetPortBits

.proc SetPortBitsImpl
        sub16   current_viewloc_x, current_maprect_x1, x_offset
        sub16   current_viewloc_y, current_maprect_y1, y_offset
        rts
.endproc


.proc clip_rect
        lda     current_maprect_x2+1
        cmp     left+1
        bmi     fail
        bne     in_left
        lda     current_maprect_x2
        cmp     left
        bcs     in_left
fail:   clc
fail2:  rts

in_left:
        lda     right+1
        cmp     current_maprect_x1+1
        bmi     fail
        bne     in_right
        lda     right
        cmp     current_maprect_x1
        bcc     fail2

in_right:
        lda     current_maprect_y2+1
        cmp     top+1
        bmi     fail
        bne     in_bottom
        lda     current_maprect_y2
        cmp     top
        bcc     fail2

in_bottom:
        lda     bottom+1
        cmp     current_maprect_y1+1
        bmi     fail
        bne     in_top
        lda     bottom
        cmp     current_maprect_y1
        bcc     fail2

in_top: ldy     #0
        lda     left
        sec
        sbc     current_maprect_x1
        tax
        lda     left+1
        sbc     current_maprect_x1+1
        bpl     :+

        stx     clipped_left
        sta     clipped_left+1
        copy16  current_maprect_x1, left
        iny

:       lda     current_maprect_x2
        sec
        sbc     right
        tax
        lda     current_maprect_x2+1
        sbc     right+1
        bpl     :+

        copy16  current_maprect_x2, right
        tya
        ora     #$04
        tay

:       lda     top
        sec
        sbc     current_maprect_y1
        tax
        lda     top+1
        sbc     current_maprect_y1+1
        bpl     :+

        stx     clipped_top
        sta     clipped_top+1
        copy16  current_maprect_y1, top
        iny
        iny

:       lda     current_maprect_y2
        sec
        sbc     bottom
        tax
        lda     current_maprect_y2+1
        sbc     bottom+1
        bpl     :+
        copy16  current_maprect_y2, bottom
        tya
        ora     #$08
        tay

:       sty     $9A
        sec
        rts
.endproc


.proc check_rect
        sec
        lda     right
        sbc     left
        lda     right+1
        sbc     left+1
        bmi     bad_rect
        sec
        lda     bottom
        sbc     top
        lda     bottom+1
        sbc     top+1
        bmi     bad_rect
        rts

bad_rect:
        exit_call MGTK::error_empty_object
.endproc

        .include "rect-exp.inc"

