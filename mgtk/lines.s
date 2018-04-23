;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        MGTK_IMPORT PaintPolyImpl
        MGTK_IMPORT draw_line


;;; ============================================================
;;; Move

;;; 4 bytes of params, copied to $A1

.proc MoveImpl
        params := $A1
        xdelta := params
        ydelta := params + 2

        ldax    xdelta
        jsr     adjust_xpos
        ldax    ydelta
        clc
        adc     current_penloc_y
        sta     current_penloc_y
        txa
        adc     current_penloc_y+1
        sta     current_penloc_y+1
        rts
.endproc

        ;; Adjust current_penloc_x by (X,A)
.proc adjust_xpos
        clc
        adc     current_penloc_x
        sta     current_penloc_x
        txa
        adc     current_penloc_x+1
        sta     current_penloc_x+1
        rts
.endproc

;;; ============================================================
;;; LineImpl

;;; 4 bytes of params, copied to $A1

.proc LineImpl
        params := $A1
        xdelta := params
        ydelta := params + 2

        ldx     #2              ; Convert relative x/y to absolute x/y at $92,$94
loop:   add16   xdelta,x, current_penloc_x,x, $92,x
        dex
        dex
        bpl     loop
        ;; fall through
.endproc

;;; ============================================================
;;; LineTo

;;; 4 bytes of params, copied to $92

.proc LineToImpl
        params  := $92
        xend    := params + 0
        yend    := params + 2

        pt1     := $92
        x1      := pt1
        y1      := pt1+2

        pt2     := $96
        x2      := pt2
        y2      := pt2+2

        loop_ctr := $82
        temp_pt  := $83


        ldx     #3
:       lda     current_penloc,x     ; move pos to $96, assign params to pos
        sta     pt2,x
        lda     pt1,x
        sta     current_penloc,x
        dex
        bpl     :-

        ;; Called from elsewhere; draw $92,$94 to $96,$98; values modified
do_draw_line:
        lda     y2+1
        cmp     y1+1
        bmi     swap_start_end
        bne     L57BF
        lda     y2
        cmp     y1
        bcc     swap_start_end
        bne     L57BF

        ;; y1 == y2
        lda     x1
        ldx     x1+1
        cpx     x2+1
        bmi     draw_line_jmp
        bne     :+
        cmp     x2
        bcc     draw_line_jmp

:       ldy     x2              ; swap so x1 < x2
        sta     x2
        sty     x1
        ldy     x2+1
        stx     x2+1
        sty     x1+1
draw_line_jmp:
        jmp     draw_line

swap_start_end:
        ldx     #3              ; Swap start/end
:       lda     pt1,x
        tay
        lda     pt2,x
        sta     pt1,x
        tya
        sta     pt2,x
        dex
        bpl     :-

L57BF:  ldx     current_penwidth
        dex
        stx     $A2
        lda     current_penheight
        sta     $A4
        lda     #0
        sta     $A1
        sta     $A3

        lda     x1
        ldx     x1+1
        cpx     x2+1
        bmi     L57E9
        bne     L57E1
        cmp     x2
        bcc     L57E9
        bne     L57E1
        jmp     draw_line

L57E1:  lda     $A1
        ldx     $A2
        sta     $A2
        stx     $A1

L57E9:  ldy     #5                ; do 6 points
loop:   sty     loop_ctr
        ldx     pt_offsets,y      ; offset into the pt1,pt2 structure
        ldy     #3
:       lda     pt1,x
        sta     temp_pt,y
        dex
        dey
        bpl     :-

        ldy     loop_ctr
        ldx     penwidth_flags,y  ; when =1, will add the current_penwidth
        lda     $A1,x
        clc
        adc     temp_pt
        sta     temp_pt
        bcc     :+
        inc     temp_pt+1
:
        ldx     penheight_flags,y ; when =2, will add the current_penheight
        lda     $A3,x
        clc
        adc     temp_pt+2
        sta     temp_pt+2
        bcc     :+
        inc     temp_pt+3
:
        tya
        asl     a
        asl     a
        tay

        ldx     #0
:       lda     temp_pt,x
        sta     paint_poly_points,y
        iny
        inx
        cpx     #4
        bne     :-

        ldy     loop_ctr
        dey
        bpl     loop

        copy16  paint_poly_params_addr, params_addr
        jmp     PaintPolyImpl

paint_poly_params_addr:
        .addr   paint_poly_params

;;       Points  0  1  2  3  4  5
pt_offsets:
        .byte    3, 3, 7, 7, 7, 3
penwidth_flags:
        .byte    0, 0, 0, 1, 1, 1
penheight_flags:
        .byte    0, 1, 1, 1, 0, 0

        ;; params for a PaintPoly call
paint_poly_params:
        .byte   6         ; number of points
        .byte   0
paint_poly_points:
        .res    4*6       ; points

.endproc
        DRAW_LINE_ABS_IMPL_do_draw_line := LineToImpl::do_draw_line

        .include "lines-exp.inc"

