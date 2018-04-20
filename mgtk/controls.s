;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        .import FindWindowImpl__return_result
        .import HideCursorImpl
        .import ShowCursorImpl

        .import check_if_changed
        .importzp current_winfo
        .importzp current_winfo__hscroll
        .importzp current_winfo__options
        .importzp current_winfo__vscroll
        .import drag_curpos
        .import drag_delta
        .import drag_initialpos
        .import fill_and_frame_rect
        .import fixed_div
        .importzp fixed_div_dividend
        .importzp fixed_div_divisor
        .importzp fixed_div_quotient
        .import frame_winrect
        .import get_and_return_event
        .import get_win_horizscrollrect
        .import get_win_vertscrollrect
        .import get_winrect
        .import hide_cursor_save_params
        .import in_winrect
        .import return_winrect
        .import save_params_and_stack
        .import set_desktop_port
        .import set_fill_mode
        .import set_standard_port
        .import show_cursor_and_restore
        .import standard_port__penpattern
        .import store_xa_at_y
        .import top_window
        .importzp which_control
        .importzp which_control_horiz
        .importzp which_control_vert
        .importzp window
        .importzp winrect
        .importzp winrect__x1
        .importzp winrect__y1
        .importzp winrect__x2
        .importzp winrect__y2

        .import MGTK__PaintRect
        .import MGTK__SetPattern



;;; ============================================================
;;; ActivateCtl

;;; 2 bytes of params, copied to $8C

.proc ActivateCtlImpl
        PARAM_BLOCK params, $8C
which_ctl: .byte   0
activate:  .byte   0
        END_PARAM_BLOCK


        lda     which_control
        cmp     #MGTK::ctl_vertical_scroll_bar
        bne     :+

        lda     #which_control_vert
        sta     which_control
        bne     activate

:       cmp     #MGTK::ctl_horizontal_scroll_bar
        bne     ret

        lda     #which_control_horiz
        sta     which_control
        beq     activate
ret:    rts

activate:
        jsr     hide_cursor_save_params
        jsr     top_window

        bit     which_control
        bpl     :+

        lda     current_winfo__vscroll
        ldy     #MGTK::winfo_offset_vscroll
        bne     toggle

:       lda     current_winfo__hscroll
        ldy     #MGTK::winfo_offset_hscroll

toggle: eor     params::activate
        and     #1
        eor     (window),y
        sta     (window),y

        lda     params::activate
        jsr     draw_or_erase_scrollbar
        jmp     show_cursor_and_restore
.endproc


.proc draw_or_erase_scrollbar
        bne     do_draw

        jsr     get_scrollbar_scroll_area
        jsr     set_standard_port
        MGTK_CALL MGTK__PaintRect, winrect
        rts

do_draw:
        bit     which_control
        bmi     vert_scrollbar
        bit     current_winfo__hscroll
        bmi     has_scroll
ret:    rts

vert_scrollbar:
        bit     current_winfo__vscroll
        bpl     ret
has_scroll:
        jsr     set_standard_port
        jsr     get_scrollbar_scroll_area

        MGTK_CALL MGTK__SetPattern, light_speckles_pattern
        MGTK_CALL MGTK__PaintRect, winrect
        MGTK_CALL MGTK__SetPattern, standard_port__penpattern

        bit     which_control
        bmi     vert_thumb

        bit     current_winfo__hscroll
        bvs     has_thumb
ret2:   rts

vert_thumb:
        bit     current_winfo__vscroll
        bvc     ret2
has_thumb:
        jsr     get_thumb_rect
        jmp     fill_and_frame_rect
.endproc


light_speckles_pattern:
        .byte   %11011101
        .byte   %01110111
        .byte   %11011101
        .byte   %01110111
        .byte   %11011101
        .byte   %01110111
        .byte   %11011101
        .byte   %01110111

        .byte   $00,$00


.proc get_scrollbar_scroll_area
        bit     which_control
        bpl     horiz

        jsr     get_win_vertscrollrect
        lda     winrect__y1
        clc
.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        adc     #12
.else
        adc     #11
.endif
        sta     winrect__y1
        bcc     :+
        inc     winrect__y1+1
:
        lda     winrect__y2
        sec
        sbc     #$0B
        sta     winrect__y2
        bcs     :+
        dec     winrect__y2+1
:
        lda     current_winfo__options
        and     #MGTK::option_grow_box
        bne     :+

        bit     current_winfo__hscroll
        bpl     v_noscroll

:       lda     winrect__y2
        sec
        sbc     #$0B
        sta     winrect__y2
        bcs     :+
        dec     winrect__y2+1
:
v_noscroll:
        inc16   winrect__x1
        lda     winrect__x2
        bne     :+
        dec     winrect__x2+1
:       dec     winrect__x2
        jmp     return_winrect_jmp


horiz:  jsr     get_win_horizscrollrect
        lda     winrect__x1
        clc
        adc     #$15
        sta     winrect__x1
        bcc     :+
        inc     winrect__x1+1
:
        lda     winrect__x2
        sec
        sbc     #$15
        sta     winrect__x2
        bcs     :+
        dec     winrect__x2+1
:
        lda     current_winfo__options
        and     #MGTK::option_grow_box
        bne     :+

        bit     current_winfo__vscroll
        bpl     h_novscroll

:       lda     winrect__x2
        sec
        sbc     #$15
        sta     winrect__x2
        bcs     h_novscroll
        dec     winrect__x2+1

h_novscroll:
        inc16   winrect__y1

        lda     winrect__y2
        bne     :+
        dec     winrect__y2+1
:       dec     winrect__y2

return_winrect_jmp:
        jmp     return_winrect
.endproc


        thumb_max := $A3
        thumb_pos := $A1

        xthumb_width := 20
        ythumb_height := 12


.proc get_thumb_rect
        thumb_coord := $82

        jsr     get_scrollbar_scroll_area

        jsr     get_thumb_vals
        jsr     fixed_div

        lda     fixed_div_quotient+2    ; 8.0 integral part
        pha
        jsr     calc_ctl_bounds
        jsr     set_up_thumb_division
        pla
        tax

        lda     thumb_max
        ldy     thumb_max+1

        cpx     #1              ; 100%
        beq     :+

        ldx     fixed_div_quotient+1    ; 0.8 fractional part
        jsr     get_thumb_coord

:       sta     thumb_coord
        sty     thumb_coord+1

        ldx     #0              ; x-coords
        lda     #xthumb_width

        bit     which_control
        bpl     :+

        ldx     #2              ; y-coords
        lda     #ythumb_height
:       pha
        add16   winrect,x, thumb_coord, winrect,x
        pla
        clc
        adc     winrect__x1,x
        sta     winrect__x2,x
        lda     winrect__x1+1,x
        adc     #0
        sta     winrect__x2+1,x
        jmp     return_winrect
.endproc

;;; ============================================================
;;; FindControl

;;; 4 bytes of params, copied to current_penloc

.proc FindControlImpl
        jsr     save_params_and_stack

        jsr     top_window
        bne     :+
        exit_call MGTK::error_no_active_window

:       bit     current_winfo__vscroll
        bpl     no_vscroll

        jsr     get_win_vertscrollrect
        jsr     in_winrect
        beq     no_vscroll

        ldx     #0
        lda     current_winfo__vscroll
        and     #$01
        beq     vscrollbar

        lda     #which_control_vert
        sta     which_control

        jsr     get_scrollbar_scroll_area
        jsr     in_winrect
        beq     in_arrows

        bit     current_winfo__vscroll
        bcs     return_dead_zone ; never ???

        jsr     get_thumb_rect
        jsr     in_winrect
        beq     no_thumb

        ldx     #MGTK::part_thumb
        bne     vscrollbar

in_arrows:
        lda     #MGTK::part_up_arrow
        bne     :+

no_thumb:
        lda     #MGTK::part_page_up
:       pha
        jsr     get_thumb_rect
        pla
        tax
        lda     current_penloc_y
        cmp     winrect__y1
        bcc     :+
        inx                     ; part_down_arrow / part_page_down
:
vscrollbar:
        lda     #MGTK::ctl_vertical_scroll_bar
        bne     return_result

no_vscroll:
        bit     current_winfo__hscroll
        bpl     no_hscroll

        jsr     get_win_horizscrollrect
        jsr     in_winrect
        beq     no_hscroll

        ldx     #0
        lda     current_winfo__hscroll
        and     #$01
        beq     hscrollbar

        lda     #which_control_horiz
        sta     which_control

        jsr     get_scrollbar_scroll_area
        jsr     in_winrect
        beq     in_harrows

        bit     current_winfo__hscroll
        bvc     return_dead_zone

        jsr     get_thumb_rect
        jsr     in_winrect
        beq     no_hthumb

        ldx     #MGTK::part_thumb
        bne     hscrollbar

in_harrows:
        lda     #MGTK::part_left_arrow
        bne     :+

no_hthumb:
        lda     #MGTK::part_page_left
:       pha
        jsr     get_thumb_rect
        pla
        tax
        lda     current_penloc_x+1
        cmp     winrect__x1+1
        bcc     hscrollbar
        bne     :+
        lda     current_penloc_x
        cmp     winrect__x1
        bcc     hscrollbar
:       inx

hscrollbar:
        lda     #MGTK::ctl_horizontal_scroll_bar
        bne     return_result

no_hscroll:
        jsr     get_winrect
        jsr     in_winrect
        beq     return_dead_zone

        lda     #MGTK::ctl_not_a_control
        beq     return_result

return_dead_zone:
        lda     #MGTK::ctl_dead_zone
return_result:
        jmp     FindWindowImpl__return_result
.endproc


;;; ============================================================
;;; SetCtlMax

;;; 3 bytes of params, copied to $82

.proc SetCtlMaxImpl
        PARAM_BLOCK params, $82
which_ctl: .byte  0
ctlmax:    .byte  0
           .byte  0      ; unknown
        END_PARAM_BLOCK

        lda     params::which_ctl
        cmp     #MGTK::ctl_vertical_scroll_bar
        bne     :+
        lda     #$80
        sta     params::which_ctl
        bne     got_ctl        ; always

:       cmp     #MGTK::ctl_horizontal_scroll_bar
        bne     :+
        lda     #$00
        sta     params::which_ctl
        beq     got_ctl        ; always

:       exit_call MGTK::error_control_not_found

got_ctl:
        jsr     top_window
        bne     :+

        exit_call MGTK::error_no_active_window

:       ldy     #MGTK::winfo_offset_hthumbmax
        bit     params::which_ctl
        bpl     :+

        ldy     #MGTK::winfo_offset_vthumbmax
:       lda     params::ctlmax
        sta     (window),y
        sta     current_winfo,y
        rts
.endproc

;;; ============================================================
;;; TrackThumb

;;; 5 bytes of params, copied to $82

.proc TrackThumbImpl
        PARAM_BLOCK params, $82
which_ctl:  .byte   0
mouse_pos:
mousex:     .word   0
mousey:     .word   0
thumbpos:   .byte   0
thumbmoved: .byte   0
        END_PARAM_BLOCK

        thumb_dim := $82


        lda     params::which_ctl
        cmp     #MGTK::ctl_vertical_scroll_bar
        bne     :+

        lda     #which_control_vert
        sta     params::which_ctl
        bne     got_ctl                    ; always

:       cmp     #MGTK::ctl_horizontal_scroll_bar
        bne     :+

        lda     #which_control_horiz
        sta     params::which_ctl
        beq     got_ctl                    ; always

:       exit_call MGTK::error_control_not_found

got_ctl:lda     params::which_ctl
        sta     which_control

        ldx     #3
:       lda     params::mouse_pos,x
        sta     drag_initialpos,x
        sta     drag_curpos,x
        dex
        bpl     :-

        jsr     top_window
        bne     :+
        exit_call MGTK::error_no_active_window

:       jsr     get_thumb_rect
        jsr     save_params_and_stack
        jsr     set_desktop_port

        lda     #MGTK::penXOR
        jsr     set_fill_mode
        MGTK_CALL MGTK__SetPattern, light_speckles_pattern

        jsr     HideCursorImpl
drag_loop:
        jsr     frame_winrect
        jsr     ShowCursorImpl

no_change:
        jsr     get_and_return_event
        cmp     #MGTK::event_kind_button_up
        beq     drag_done

        jsr     check_if_changed
        beq     no_change

        jsr     HideCursorImpl
        jsr     frame_winrect

        jsr     top_window
        jsr     get_thumb_rect

        ldx     #0
        lda     #xthumb_width

        bit     which_control
        bpl     :+

        ldx     #2
        lda     #ythumb_height

:       sta     thumb_dim

        lda     winrect,x
        clc
        adc     drag_delta,x
        tay
        lda     winrect+1,x
        adc     drag_delta+1,x
        cmp     ctl_bound1+1
        bcc     :+
        bne     in_bound1
        cpy     ctl_bound1
        bcs     in_bound1

:       lda     ctl_bound1+1
        ldy     ctl_bound1

in_bound1:
        cmp     ctl_bound2+1
        bcc     in_bound2
        bne     :+
        cpy     ctl_bound2
        bcc     in_bound2

:       lda     ctl_bound2+1
        ldy     ctl_bound2

in_bound2:
        sta     winrect+1,x
        tya
        sta     winrect,x
        clc
        adc     thumb_dim
        sta     winrect__x2,x
        lda     winrect+1,x
        adc     #0
        sta     winrect__x2+1,x
        jmp     drag_loop

drag_done:
        jsr     HideCursorImpl
        jsr     frame_winrect
        jsr     show_cursor_and_restore

        jsr     set_up_thumb_division

        jsr     fixed_div
        ldx     fixed_div_quotient+2    ; 8.0 integral part

        jsr     get_thumb_vals

        lda     fixed_div_divisor
        ldy     #0
        cpx     #1
        bcs     :+

        ldx     fixed_div_quotient+1     ; 0.8 fractional part
        jsr     get_thumb_coord

:       ldx     #1
        cmp     fixed_div_quotient+2
        bne     :+
        dex

:       ldy     #params::thumbpos - params
        jmp     store_xa_at_y
.endproc


        ;; Calculates the thumb coordinates given the maximum position
        ;; and current fraction.
        ;;
        ;; Inputs:
        ;;    A,Y = maximum position of thumb in 16.0 format
        ;;    X   = fraction in fixed 0.8 format
        ;;
        ;; Outputs:
        ;;    A,Y = current position of thumb in 16.0 format
        ;;          (= maximum position * fraction)
        ;;
.proc get_thumb_coord
        increment := $82
        accum     := $84

        sta     increment       ; fixed 8.8 = max position/256
        sty     increment+1

        lda     #$80            ; fixed 8.8 = 1/2
        sta     accum
        ldy     #$00
        sty     accum+1

        txa                     ; fixed 8.0 = fraction*256
        beq     ret
loop:   add16   increment, accum, accum ; returning with A=high byte of accum
        bcc     :+
        iny
:       dex                     ; (accum low),A,Y is in fixed 16.8
        bne     loop            ; return A,Y
ret:    rts
.endproc


ctl_bound2:
        .res 2
ctl_bound1:
        .res 2


        ;; Set fixed_div::divisor and fixed_div::dividend up for the
        ;; proportion calculation for the control in which_control.
.proc set_up_thumb_division
        sub16   ctl_bound2, ctl_bound1, fixed_div_divisor
        ldx     #0
        bit     which_control
        bpl     :+

        ldx     #2
:       sub16   winrect,x, ctl_bound1, fixed_div_dividend
        rts
.endproc


        ;; Set thumb_max and thumb_pos according to the control indicated
        ;; in which_control.
.proc get_thumb_vals
        ldy     #MGTK::winfo_offset_hthumbmax

        bit     which_control
        bpl     is_horiz

        ldy     #MGTK::winfo_offset_vthumbmax
is_horiz:
        lda     (window),y
        sta     thumb_max
        iny
        lda     (window),y
        sta     thumb_pos

        lda     #0
        sta     thumb_pos+1
        sta     thumb_max+1
        rts
.endproc


.proc calc_ctl_bounds
        offset := $82

        ldx     #0
        lda     #xthumb_width

        bit     which_control
        bpl     :+

        ldx     #2
        lda     #ythumb_height
:
        sta     offset

        lda     winrect__x1,x
        ldy     winrect__x1+1,x
        sta     ctl_bound1
        sty     ctl_bound1+1

        lda     winrect__x2,x
        ldy     winrect__x2+1,x
        sec
        sbc     offset
        bcs     :+
        dey
:       sta     ctl_bound2
        sty     ctl_bound2+1
        rts
.endproc


;;; ============================================================
;;; UpdateThumb

;;; 3 bytes of params, copied to $8C

.proc UpdateThumbImpl
        PARAM_BLOCK params, $8C
which_ctl:  .byte   0
thumbpos:   .byte   0
            .byte   0      ; unknown
        END_PARAM_BLOCK


        lda     which_control
        cmp     #MGTK::ctl_vertical_scroll_bar
        bne     :+
        lda     #which_control_vert
        sta     which_control
        bne     check_win

:       cmp     #MGTK::ctl_horizontal_scroll_bar
        bne     bad_ctl
        lda     #which_control_horiz
        sta     which_control
        beq     check_win

bad_ctl:
        exit_call MGTK::error_control_not_found

check_win:
        jsr     top_window
        bne     :+
        exit_call MGTK::error_no_active_window

:       ldy     #MGTK::winfo_offset_hthumbpos
        bit     which_control
        bpl     :+

        ldy     #MGTK::winfo_offset_vthumbpos
:       lda     params::thumbpos
        sta     (window),y

        jsr     hide_cursor_save_params
        jsr     set_standard_port
        jsr     draw_or_erase_scrollbar
        jmp     show_cursor_and_restore
.endproc


        ;; Control Manager API

        MGTK_DECL_API  FindControl,    $48, FindControlImpl, 0, current_penloc, 4
        MGTK_DECL_API  SetCtlMax,      $49, SetCtlMaxImpl, 0, ::params
        MGTK_DECL_API  TrackThumb,     $4A, TrackThumbImpl, 0, ::params, 5
        MGTK_DECL_API  UpdateThumb,    $4B, UpdateThumbImpl, 0, ::params
        MGTK_DECL_API  ActivateCtl,    $4C, ActivateCtlImpl, 0, ::params


        MGTK_DECL_ERROR  error_control_not_found       ; $A4


        .include "controls-exp.inc"
