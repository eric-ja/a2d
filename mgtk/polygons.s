;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        vertex_limit     := $B3
        vertices_count   := $B4
        poly_oper        := $BA       ; positive = paint; negative = test
        start_index      := $AE

        poly_oper_paint  := $00
        poly_oper_test   := $80


        ;; Text page buffers
        poly_maxima_links := $0428
        poly_maxima_prev_vertex := $0468
        poly_maxima_next_vertex := $04A8
        poly_maxima_slope0 := $0528
        poly_maxima_slope1 := $04E8
        poly_maxima_slope2 := $0568
        poly_maxima_slope3 := $05A8
        poly_maxima_yl_table := $05E8

        poly_vertex_prev_link := $0680
        poly_vertex_next_link := $06BC

        poly_xl_buffer  := $0700
        poly_xh_buffer  := $073C
        poly_yl_buffer  := $0780
        poly_yh_buffer  := $07BC


        MGTK_IMPORT poly_maxima_xl_table
        MGTK_IMPORT poly_maxima_yh_table
        MGTK_IMPORT poly_maxima_x_frach
        MGTK_IMPORT poly_maxima_x_fracl
        MGTK_IMPORT poly_maxima_xh_table

        MGTK_IMPORT fixed_div2
        MGTK_IMPORTZP fixed_div_quotient
        MGTK_IMPORTZP fixed_div_dividend
        MGTK_IMPORTZP fixed_div_divisor

        MGTK_IMPORT PaintRectImpl__do_paint
        MGTK_IMPORT InRectImpl
        MGTK_IMPORT clip_rect


.proc load_poly
        point_index      := $82
        low_point        := $A7

        max_poly_points  := 60

        stx     $B0
        asl     a
        asl     a               ; # of vertices * 4 = length
        sta     vertex_limit

        ;; Initialize rect to first point of polygon.
        ldy     #3              ; Copy params_addr... to $92... and $96...
:       lda     (params_addr),y
        sta     left,y
        sta     right,y
        dey
        bpl     :-

        copy16  top, low_point  ; y coord

        ldy     #0
        stx     start_index
loop:   stx     point_index

        lda     (params_addr),y
        sta     poly_xl_buffer,x
        pha
        iny
        lda     (params_addr),y
        sta     poly_xh_buffer,x
        tax
        pla
        iny

        cpx     left+1
        bmi     :+
        bne     in_left
        cmp     left
        bcs     in_left

:       stax    left
        bcc     in_right

in_left:
        cpx     right+1
        bmi     in_right
        bne     :+
        cmp     right
        bcc     in_right

:       stax    right

in_right:
        ldx     point_index
        lda     (params_addr),y
        sta     poly_yl_buffer,x
        pha
        iny
        lda     (params_addr),y
        sta     poly_yh_buffer,x
        tax
        pla
        iny

        cpx     top+1
        bmi     :+
        bne     in_top
        cmp     top
        bcs     in_top

:       stax    top
        bcc     in_bottom

in_top: cpx     bottom+1
        bmi     in_bottom
        bne     :+
        cmp     bottom
        bcc     in_bottom

:       stax    bottom

in_bottom:
        cpx     low_point+1
        stx     low_point+1
        bmi     set_low_point
        bne     :+
        cmp     low_point
        bcc     set_low_point
        beq     set_low_point

:       ldx     point_index
        stx     start_index

set_low_point:
        sta     low_point

        ldx     point_index
        inx
        cpx     #max_poly_points
        beq     bad_poly
        cpy     vertex_limit
        bcc     loop

        lda     top
        cmp     bottom
        bne     :+
        lda     top+1
        cmp     bottom+1
        beq     bad_poly

:       stx     vertex_limit
        bit     poly_oper
        bpl     :+
.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        sec
.endif
        rts

:       jmp     clip_rect
.endproc


.proc next_poly
        lda     vertices_count
        bpl     orts
        asl     a
        asl     a
        adc     params_addr
        sta     params_addr
        bcc     ora_2_param_bytes
        inc     params_addr+1
        ;; Fall-through
.endproc

        ;; ORAs together first two bytes at (params_addr) and stores
        ;; in $B4, then advances params_addr
ora_2_param_bytes:
        ldy     #0
        lda     (params_addr),y
        iny
        ora     (params_addr),y
        sta     vertices_count
        inc16   params_addr
        inc16   params_addr
        ldy     #$80
orts:   rts

;;; ============================================================
;;; InPoly

InPolyImpl:
        lda     #poly_oper_test
        bne     PaintPolyImpl_entry2

;;; ============================================================
;;; PaintPoly

        ;; also called from the end of LineToImpl

        num_maxima       := $AD
        max_num_maxima   := 8

        low_vertex       := $B0


.proc PaintPolyImpl

        lda     #poly_oper_paint
entry2: sta     poly_oper
        ldx     #0
        stx     num_maxima
        jsr     ora_2_param_bytes

loop:   jsr     load_poly
        bcs     process_poly
        ldx     low_vertex
next:   jsr     next_poly
        bmi     loop

        jmp     fill_polys

bad_poly:
        exit_call MGTK::error_bad_object
.endproc


        temp_yh        := $83
        next_vertex    := $AA
        current_vertex := $AC
        loop_ctr       := $AF


.proc process_poly
        ldy     #1
        sty     loop_ctr         ; do 2 iterations of the following loop

        ldy     start_index      ; starting vertex
        cpy     low_vertex       ; lowest vertex
        bne     :+
        ldy     vertex_limit     ; highest vertex
:       dey
        sty     $AB              ; one before starting vertex

        php
loop:   sty     current_vertex   ; current vertex
        iny
        cpy     vertex_limit
        bne     :+
        ldy     low_vertex

:       sty     next_vertex      ; next vertex
        cpy     start_index      ; have we come around complete circle?
        bne     :+
        dec     loop_ctr         ; this completes one loop

:       lda     poly_yl_buffer,y
        ldx     poly_yh_buffer,y
        stx     temp_yh
vloop:  sty     $A9              ; starting from next vertex, search ahead
        iny                      ; for a subsequent vertex with differing y
        cpy     vertex_limit
        bne     :+
        ldy     low_vertex
:
        cmp     poly_yl_buffer,y
        bne     :+
        ldx     poly_yh_buffer,y
        cpx     temp_yh
        beq     vloop

:       ldx     $AB              ; find y difference with current vertex
        sec
        sbc     poly_yl_buffer,x
        lda     temp_yh
        sbc     poly_yh_buffer,x
        bmi     y_less

        lda     $A9              ; vertex before new vertex

        plp                      ; check maxima flag
        bmi     new_maxima       ; if set, go create new maxima

        tay
        sta     poly_vertex_prev_link,x   ; link current vertex -> vertex before new vertex
        lda     next_vertex
        sta     poly_vertex_next_link,x   ; link current vertex -> next vertex
        bpl     next

new_maxima:
        ldx     num_maxima
        cpx     #2*max_num_maxima         ; too many maxima points (documented limitation)
        bcs     bad_poly

        sta     poly_maxima_prev_vertex,x ; vertex before new vertex
        lda     next_vertex
        sta     poly_maxima_next_vertex,x ; current vertex

        ldy     $AB
        lda     poly_vertex_prev_link,y
        sta     poly_maxima_prev_vertex+1,x
        lda     poly_vertex_next_link,y
        sta     poly_maxima_next_vertex+1,x

        lda     poly_yl_buffer,y
        sta     poly_maxima_yl_table,x
        sta     poly_maxima_yl_table+1,x

        lda     poly_yh_buffer,y
        sta     poly_maxima_yh_table,x
        sta     poly_maxima_yh_table+1,x

        lda     poly_xl_buffer,y
        sta     poly_maxima_xl_table+1,x
        lda     poly_xh_buffer,y
        sta     poly_maxima_xh_table+1,x

        ldy     current_vertex
        lda     poly_xl_buffer,y
        sta     poly_maxima_xl_table,x
        lda     poly_xh_buffer,y
        sta     poly_maxima_xh_table,x
        inx
        inx
        stx     num_maxima
        ldy     $A9
        bpl     next

y_less: plp                         ; check maxima flag
        bmi     :+
        lda     #$80
        sta     poly_vertex_prev_link,x             ; link current vertex -> #$80

:       ldy     next_vertex
        txa
        sta     poly_vertex_prev_link,y             ; link next vertex -> current vertex
        lda     current_vertex
        sta     poly_vertex_next_link,y
        lda     #$80                ; set negative flag so next iteration captures a maxima

next:   php
        sty     $AB
        ldy     $A9
        bit     loop_ctr
        bmi     :+
        jmp     loop

:       plp
        ldx     vertex_limit
        jmp     PaintPolyImpl::next
.endproc


        scan_y       := $A9
        lr_flag      := $AB
        start_maxima := $B1

.proc fill_polys
        ldx     #0
        stx     start_maxima
        lda     #$80
        sta     poly_maxima_links
        sta     $B2

loop:   inx
        cpx     num_maxima
        bcc     :+
        beq     links_done
        rts

:       lda     start_maxima
next_link:
        tay
        lda     poly_maxima_yl_table,x
        cmp     poly_maxima_yl_table,y
        bcs     x_ge_y
        tya                            ; poly_maxima_y[xReg] < poly_maxima_y[yReg]
        sta     poly_maxima_links,x    ; then xReg linked to yReg
        cpy     start_maxima
        beq     :+                     ; if yReg was the start, set the start to xReg
        ldy     $82
        txa
        sta     poly_maxima_links,y    ; else $82 linked to xReg
        jmp     loop

:       stx     start_maxima           ; set start to xReg
        bcs     loop                   ; always

x_ge_y: sty     $82                    ; poly_maxima_y[xReg] >= poly_maxima_y[yReg]
        lda     poly_maxima_links,y
        bpl     next_link              ; if yReg was the end
        sta     poly_maxima_links,x    ; then set xReg as end
        txa
        sta     poly_maxima_links,y    ; and link yReg to xReg
        bpl     loop                   ; always
links_done:

        ldx     start_maxima
        lda     poly_maxima_yl_table,x
        sta     scan_y
        sta     top
        lda     poly_maxima_yh_table,x
        sta     scan_y+1
        sta     top+1

scan_loop:
        ldx     start_maxima
        bmi     L5534
scan_next:
        lda     poly_maxima_yl_table,x
        cmp     scan_y
        bne     L5532
        lda     poly_maxima_yh_table,x
        cmp     scan_y+1
        bne     L5532

        lda     poly_maxima_links,x
        sta     $82
        jsr     calc_slope

        lda     $B2
        bmi     L5517

L54E0:  tay
        lda     poly_maxima_xh_table,x
        cmp     poly_maxima_xh_table,y
        bmi     L5520
        bne     :+

        lda     poly_maxima_xl_table,x
        cmp     poly_maxima_xl_table,y
        bcc     L5520
        bne     :+

        lda     poly_maxima_x_frach,x
        cmp     poly_maxima_x_frach,y
        bcc     L5520
        bne     :+

        lda     poly_maxima_x_fracl,x
        cmp     poly_maxima_x_fracl,y
        bcc     L5520

:       sty     $83
        lda     poly_maxima_links,y
        bpl     L54E0
        sta     poly_maxima_links,x
        txa
        sta     poly_maxima_links,y
        bpl     L552E

L5517:  sta     poly_maxima_links,x
        stx     $B2
        jmp     L552E

done:   rts

L5520:  tya
        cpy     $B2
        beq     L5517
        sta     poly_maxima_links,x
        txa
        ldy     $83
        sta     poly_maxima_links,y

L552E:  ldx     $82
        bpl     scan_next

L5532:  stx     $B1
L5534:  lda     #0
        sta     lr_flag

        lda     $B2
        sta     $83
        bmi     done

scan_loop2:
        tax
        lda     scan_y
        cmp     poly_maxima_yl_table,x
        bne     scan_point
        lda     scan_y+1
        cmp     poly_maxima_yh_table,x
        bne     scan_point

        ldy     poly_maxima_prev_vertex,x
        lda     poly_vertex_prev_link,y
        bpl     shift_point

        cpx     $B2
        beq     :+

        ldy     $83
        lda     poly_maxima_links,x
        sta     poly_maxima_links,y
        jmp     scan_next_link

:       lda     poly_maxima_links,x
        sta     $B2
        jmp     scan_next_link

shift_point:
        sta     poly_maxima_prev_vertex,x
        lda     poly_xl_buffer,y
        sta     poly_maxima_xl_table,x
        lda     poly_xh_buffer,y
        sta     poly_maxima_xh_table,x
        lda     poly_vertex_next_link,y
        sta     poly_maxima_next_vertex,x

        jsr     calc_slope

scan_point:
        stx     current_vertex
        ldy     poly_maxima_xh_table,x
        lda     poly_maxima_xl_table,x
        tax

        lda     lr_flag            ; alternate flag left/right
        eor     #$FF
        sta     lr_flag
        bpl     :+

        stx     left
        sty     left+1
        bmi     skip_rect

:       stx     right
        sty     right+1

        cpy     left+1
        bmi     :+
        bne     no_swap_lr
        cpx     left
        bcs     no_swap_lr

:       lda     left
        stx     left
        sta     right
        lda     left+1
        sty     left+1
        sta     right+1

no_swap_lr:
        lda     scan_y
        sta     top
        sta     bottom
        lda     scan_y+1
        sta     top+1
        sta     bottom+1

        bit     poly_oper
        bpl     do_paint

        jsr     InRectImpl
        jmp     skip_rect

do_paint:
        jsr     PaintRectImpl__do_paint

skip_rect:
        ldx     current_vertex

        lda     poly_maxima_x_fracl,x
        clc
        adc     poly_maxima_slope0,x
        sta     poly_maxima_x_fracl,x
        lda     poly_maxima_x_frach,x
        adc     poly_maxima_slope1,x
        sta     poly_maxima_x_frach,x

        lda     poly_maxima_xl_table,x
        adc     poly_maxima_slope2,x
        sta     poly_maxima_xl_table,x
        lda     poly_maxima_xh_table,x
        adc     poly_maxima_slope3,x
        sta     poly_maxima_xh_table,x

        lda     poly_maxima_links,x
scan_next_link:
        bmi     :+
        jmp     scan_loop2

:       inc16   scan_y
        jmp     scan_loop
.endproc


.proc calc_slope
        index   := $84

        ldy     poly_maxima_next_vertex,x

        lda     poly_yl_buffer,y
        sta     poly_maxima_yl_table,x
        sec
        sbc     scan_y
        sta     <fixed_div_divisor
        lda     poly_yh_buffer,y
        sta     poly_maxima_yh_table,x
        sbc     scan_y+1
        sta     <fixed_div_divisor+1

        lda     poly_xl_buffer,y
        sec
        sbc     poly_maxima_xl_table,x
        sta     <fixed_div_dividend
        lda     poly_xh_buffer,y
        sbc     poly_maxima_xh_table,x
        sta     <fixed_div_dividend+1

        php
        bpl     :+
        sub16   #0, fixed_div_dividend, fixed_div_dividend

:       stx     index
        jsr     fixed_div2
        ldx     index
        plp
        bpl     :+

        sub16   #0, fixed_div_quotient, fixed_div_quotient
        lda     #0
        sbc     fixed_div_quotient+2
        sta     fixed_div_quotient+2
        lda     #0
        sbc     fixed_div_quotient+3
        sta     fixed_div_quotient+3

:       lda     fixed_div_quotient+3
        sta     poly_maxima_slope3,x
        cmp     #$80
        ror     a
        pha
        lda     fixed_div_quotient+2
        sta     poly_maxima_slope2,x
        ror     a
        pha
        lda     fixed_div_quotient+1
        sta     poly_maxima_slope1,x
        ror     a
        pha
        lda     fixed_div_quotient
        sta     poly_maxima_slope0,x
        ror     a
        sta     poly_maxima_x_fracl,x
        pla
        clc
        adc     #$80
        sta     poly_maxima_x_frach,x

        pla
        adc     poly_maxima_xl_table,x
        sta     poly_maxima_xl_table,x
        pla
        adc     poly_maxima_xh_table,x
        sta     poly_maxima_xh_table,x
        rts
.endproc

PaintPolyImpl_entry2 := PaintPolyImpl::entry2
bad_poly := PaintPolyImpl::bad_poly


        .include "polygons-exp.inc"
