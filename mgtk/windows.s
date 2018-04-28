;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        MGTK_IMPORT FlushEventsImpl
        MGTK_IMPORT HideCursorImpl
        MGTK_IMPORT PaintRectImpl
        MGTK_IMPORT ShowCursorImpl
        MGTK_IMPORT StartDeskTopImpl__reset_desktop

        MGTK_IMPORT apply_active_port_to_port
        MGTK_IMPORT apply_port_to_active_port
        MGTK_IMPORT assign_and_prepare_port
        MGTK_IMPORT checkerboard_pattern
        MGTK_IMPORT clip_rect
        MGTK_IMPORT do_measure_text
        MGTK_IMPORT draw_icon
        MGTK_IMPORT draw_or_erase_scrollbar
        MGTK_IMPORT draw_text
        MGTK_IMPORT eventbuf__kind
        MGTK_IMPORT eventbuf__window_id
        MGTK_IMPORT fill_and_frame_rect
        MGTK_IMPORT fill_rect_params
        MGTK_IMPORT finish_grow
        MGTK_IMPORT force_tracking_change
        MGTK_IMPORT get_and_return_event
        MGTK_IMPORTZP get_and_return_event__event__mouse_pos
        MGTK_IMPORT grew_flag
        MGTK_IMPORT hide_cursor_save_params
        MGTK_IMPORT kbd_mouse_init_tracking
        MGTK_IMPORT kbd_mouse_state
        MGTK_IMPORT kbd_win_drag_or_grow
        MGTK_IMPORT movement_cancel
        MGTK_IMPORT penloc_to_bounds
        MGTK_IMPORT preserve_zp_flag
        MGTK_IMPORT put_event
        MGTK_IMPORT restore_params_active_port
        MGTK_IMPORT save_params_and_stack
        MGTK_IMPORT set_and_prepare_port
        MGTK_IMPORT set_desktop_port
        MGTK_IMPORT set_fill_mode
        MGTK_IMPORT set_grew_flag
        MGTK_IMPORT set_input
        MGTK_IMPORT set_pos_params
        MGTK_IMPORT show_cursor_and_restore
        MGTK_IMPORT standard_port__penpattern
        MGTK_IMPORT store_xa_at_y
        MGTK_IMPORT test_rect_params

        MGTK_IMPORTZP winrect
        MGTK_IMPORTZP winrect__x1
        MGTK_IMPORTZP winrect__y1
        MGTK_IMPORTZP winrect__x2
        MGTK_IMPORTZP winrect__y2


icon_offset_pos    := 0
icon_offset_width  := 4
icon_offset_height := 5
icon_offset_bits   := 6


.proc up_scroll_params
xcoord: .res    2
ycoord: .res    2
        .byte   19,10
        .addr   up_scroll_bitmap
.endproc

.proc down_scroll_params
xcoord: .res    2
ycoord: .res    2
        .byte   19,10
        .addr   down_scroll_bitmap
.endproc

.proc left_scroll_params
xcoord: .res    2
ycoord: .res    2
        .byte   20,9
        .addr   left_scroll_bitmap
.endproc

.proc right_scroll_params
xcoord: .res    2
ycoord: .res    2
        .byte   18,9
        .addr   right_scroll_bitmap
.endproc

.proc resize_box_params
xcoord: .res    2
ycoord: .res    2
        .byte   20,10
        .addr   resize_box_bitmap
.endproc

        ;;  Up Scroll
up_scroll_bitmap:
        .byte   px(%0000000),px(%0000000),px(%0000000)
        .byte   px(%0000000),px(%0001100),px(%0000000)
        .byte   px(%0000000),px(%0110011),px(%0000000)
        .byte   px(%0000001),px(%1000000),px(%1100000)
        .byte   px(%0000110),px(%0000000),px(%0011000)
        .byte   px(%0011111),px(%1000000),px(%1111110)
        .byte   px(%0000001),px(%1000000),px(%1100000)
        .byte   px(%0000001),px(%1000000),px(%1100000)
        .byte   px(%0000001),px(%1111111),px(%1100000)
        .byte   px(%0000000),px(%0000000),px(%0000000)
        .byte   px(%0111111),px(%1111111),px(%1111111)

        ;; Down Scroll
down_scroll_bitmap:
        .byte   px(%0111111),px(%1111111),px(%1111111)
        .byte   px(%0000000),px(%0000000),px(%0000000)
        .byte   px(%0000001),px(%1111111),px(%1100000)
        .byte   px(%0000001),px(%1000000),px(%1100000)
        .byte   px(%0000001),px(%1000000),px(%1100000)
        .byte   px(%0011111),px(%1000000),px(%1111110)
        .byte   px(%0000110),px(%0000000),px(%0011000)
        .byte   px(%0000001),px(%1000000),px(%1100000)
        .byte   px(%0000000),px(%0110011),px(%0000000)
        .byte   px(%0000000),px(%0001100),px(%0000000)
        .byte   px(%0000000),px(%0000000),px(%0000000)

        ;;  Left Scroll
left_scroll_bitmap:
        .byte   px(%0000000),px(%0000000),px(%0000000)
        .byte   px(%0000000),px(%0001100),px(%0000001)
        .byte   px(%0000000),px(%0111100),px(%0000001)
        .byte   px(%0000001),px(%1001111),px(%1111001)
        .byte   px(%0000110),px(%0000000),px(%0011001)
        .byte   px(%0011000),px(%0000000),px(%0011001)
        .byte   px(%0000110),px(%0000000),px(%0011001)
        .byte   px(%0000001),px(%1001111),px(%1111001)
        .byte   px(%0000000),px(%0111100),px(%0000001)
        .byte   px(%0000000),px(%0001100),px(%0000001)

        ;; Right Scroll
right_scroll_bitmap:
        .byte   px(%0000000),px(%0000000),px(%0000000)
        .byte   px(%1000000),px(%0011000),px(%0000000)
        .byte   px(%1000000),px(%0011110),px(%0000000)
        .byte   px(%1001111),px(%1111001),px(%1000000)
        .byte   px(%1001100),px(%0000000),px(%0110000)
        .byte   px(%1001100),px(%0000000),px(%0001100)
        .byte   px(%1001100),px(%0000000),px(%0110000)
        .byte   px(%1001111),px(%1111001),px(%1000000)
        .byte   px(%1000000),px(%0011110),px(%0000000)
        .byte   px(%1000000),px(%0011000),px(%0000000)

        .byte   0         ; unreferenced ???

        ;; Resize Box
resize_box_bitmap:
        .byte   px(%1111111),px(%1111111),px(%1111111)
        .byte   px(%1000000),px(%0000000),px(%0000001)
        .byte   px(%1001111),px(%1111110),px(%0000001)
        .byte   px(%1001100),px(%0000111),px(%1111001)
        .byte   px(%1001100),px(%0000110),px(%0011001)
        .byte   px(%1001100),px(%0000110),px(%0011001)
        .byte   px(%1001111),px(%1111110),px(%0011001)
        .byte   px(%1000011),px(%0000000),px(%0011001)
        .byte   px(%1000011),px(%1111111),px(%1111001)
        .byte   px(%1000000),px(%0000000),px(%0000001)
        .byte   px(%1111111),px(%1111111),px(%1111111)

up_scroll_addr:
        .addr   up_scroll_params

down_scroll_addr:
        .addr   down_scroll_params

left_scroll_addr:
        .addr   left_scroll_params

right_scroll_addr:
        .addr   right_scroll_params

resize_box_addr:
        .addr   resize_box_params

current_window:
        .word   0

sel_window_id:
        .byte   0

found_window:
        .word   0

target_window_id:
        .byte   0

        ;; The root window is not a real window, but a structure whose
        ;; nextwinfo field lines up with current_window.
root_window_addr:
        .addr   current_window - MGTK::winfo_offset_nextwinfo


        which_control        := $8C
        which_control_horiz  := $00
        which_control_vert   := $80

        previous_window      := $A7
        window               := $A9


        ;; First 12 bytes of winfo only
        PARAM_BLOCK current_winfo, $AB
id:        .byte   0
options:   .byte   0
title:     .addr   0
hscroll:   .byte   0
vscroll:   .byte   0
hthumbmax: .byte   0
hthumbpos: .byte   0
vthumbmax: .byte   0
vthumbpos: .byte   0
status:    .byte   0
reserved:  .byte   0
        END_PARAM_BLOCK


        ;; First 16 bytes of win's grafport only
        PARAM_BLOCK current_winport, $B7
.proc viewloc
xcoord:    .word   0
ycoord:    .word   0
.endproc

mapbits:   .addr   0
mapwidth:  .byte   0
           .byte   0

.proc maprect
x1:        .word   0
y1:        .word   0
x2:        .word   0
y2:        .word   0
.endproc
        END_PARAM_BLOCK


        ;; Start window enumeration at top ???
.proc top_window
        copy16  root_window_addr, previous_window
        ldax    current_window
        bne     set_found_window
end:    rts
.endproc

        ;; Look up next window in chain. $A9/$AA will point at
        ;; window params block (also returned in X,A).
.proc next_window
        copy16  window, previous_window
        ldy     #MGTK::winfo_offset_nextwinfo+1
        lda     (window),y
        beq     top_window::end  ; if high byte is 0, end of chain
        tax
        dey
        lda     (window),y
set_found_window:
        stax    found_window
        ;; Fall-through
.endproc

        ;; Load/refresh the ZP window data areas at $AB and $B7.
.proc get_window
        ldax    found_window
get_from_ax:
        stax    window

        ldy     #11             ; copy first 12 bytes of window defintion to
:       lda     (window),y         ; to $AB
        sta     current_winfo,y
        dey
        bpl     :-

        ; copy first 16 bytes of grafport to $B7
        ldy     #MGTK::winfo_offset_port + MGTK::grafport_offset_pattern-1
:       lda     (window),y
        sta     current_winport - MGTK::winfo_offset_port,y
        dey
        cpy     #MGTK::winfo_offset_port-1
        bne     :-

return_window:
        ldax    window
        rts
.endproc
        set_found_window := next_window::set_found_window


        ;; Look up window state by id (in $82); $A9/$AA will point at
        ;; winfo (also X,A).
.proc window_by_id
        jsr     top_window
        beq     end
loop:   lda     current_winfo::id
        cmp     $82
        beq     get_window::return_window
        jsr     next_window
        bne     loop
end:    rts
.endproc

        ;; Look up window state by id (in $82); $A9/$AA will point at
        ;; winfo (also X,A).
        ;; This will exit the MGTK call directly (restoring stack, etc)
        ;; if the window is not found.
.proc window_by_id_or_exit
        jsr     window_by_id
        beq     nope
        rts
nope:   exit_call MGTK__error_window_not_found
.endproc


frame_winrect:
        MGTK_CALL MGTK::FrameRect, winrect
        rts

in_winrect:
        MGTK_CALL MGTK::InRect, winrect
        rts

        ;; Retrieve the rectangle of the current window and put it in winrect.
        ;;
        ;; The rectangle is defined by placing the top-left corner at the viewloc
        ;; of the window and setting the width and height matching the width
        ;; and height of the maprect of the window's port.
        ;;
.proc get_winrect
        ldx     #3
:       lda     current_winport::viewloc,x ; copy viewloc to left/top of winrect
        sta     winrect,x
        dex
        bpl     :-

        ldx     #2
:       lda     current_winport::maprect::x2,x ; x2/y2
        sec
        sbc     current_winport::maprect::x1,x ; x1/y1
        tay
        lda     current_winport::maprect::x2+1,x
        sbc     current_winport::maprect::x1+1,x
        pha

        tya
        clc
        adc     winrect__x1,x   ; x1/y1
        sta     winrect__x2,x   ; x2/y2
        pla
        adc     winrect__x1+1,x
        sta     winrect__x2+1,x
        dex
        dex
        bpl     :-
        ;; Fall-through
.endproc
return_winrect:
        ldax    #winrect
        rts


        ;; Return the window's rect including framing: title bar and scroll
        ;; bars.
.proc get_winframerect
        jsr     get_winrect
        lda     winrect
        bne     :+
        dec     winrect+1
:       dec     winrect

        bit     current_winfo::vscroll
        bmi     vert_scroll

        lda     current_winfo::options
        and     #MGTK::option_grow_box
        bne     vert_scroll
        lda     #$01
        bne     :+

vert_scroll:
        lda     #$15
:       clc
        adc     winrect__x2
        sta     winrect__x2
        bcc     :+
        inc     winrect__x2+1
:       lda     #1

        bit     current_winfo::hscroll
        bpl     :+

        lda     #$0B
:       clc
        adc     winrect__y2
        sta     winrect__y2
        bcc     :+
        inc     winrect__y2+1
:
        lda     #MGTK::option_dialog_box
        and     current_winfo::options
        bne     :+
        lda     winframe_top
:       sta     $82

        lda     winrect__y1
        sec
        sbc     $82
        sta     winrect__y1
        bcs     return_winrect
        dec     winrect__y1+1
        bcc     return_winrect
.endproc


.proc get_win_vertscrollrect
        jsr     get_winframerect
        ldax    winrect__x2
        sec
        sbc     #$14
        bcs     :+
        dex
:       stax    winrect__x1

        lda     current_winfo::options
        and     #MGTK::option_dialog_box
        bne     return_winrect

        lda     winrect__y1
        clc
        adc     wintitle_height
        sta     winrect__y1
        bcc     return_winrect
        inc     winrect__y1+1
        bcs     return_winrect
.endproc


.proc get_win_horizscrollrect
        jsr     get_winframerect
get_rect:
        ldax    winrect__y2
        sec
        sbc     #$0A
        bcs     :+
        dex
:       stax    winrect__y1
        jmp     return_winrect
.endproc


.proc get_win_growboxrect
        jsr     get_win_vertscrollrect
        jmp     get_win_horizscrollrect::get_rect
.endproc


.proc get_wintitlebar_rect
        jsr     get_winframerect

        lda     winrect__y1
        clc
        adc     wintitle_height
        sta     winrect__y2
        lda     winrect__y1+1
        adc     #0
        sta     winrect__y2+1

        jmp     return_winrect
.endproc


.proc get_wingoaway_rect
        jsr     get_wintitlebar_rect

        ldax    winrect__x1
        clc
.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        adc     #12
.else
        adc     #2
.endif
        bcc     :+
        inx
:       stax    winrect__x1
        clc
        adc     #14
        bcc     :+
        inx
:       stax    winrect__x2

        ldax    winrect__y1
        clc
        adc     #2
        bcc     :+
        inx
:       stax    winrect__y1
        clc
        adc     goaway_height
        bcc     :+
        inx
:       stax    winrect__y2

        jmp     return_winrect
.endproc


.proc draw_window
        jsr     get_winframerect
        jsr     fill_and_frame_rect

        lda     current_winfo::options
        and     #MGTK::option_dialog_box
        bne     no_titlebar

        jsr     get_wintitlebar_rect
        jsr     fill_and_frame_rect
        jsr     center_title_text

        ldax    current_winfo::title
        jsr     draw_text

no_titlebar:
        jsr     get_window

        bit     current_winfo::vscroll
        bpl     no_vert_scroll

        jsr     get_win_vertscrollrect
        jsr     frame_winrect

no_vert_scroll:
        bit     current_winfo::hscroll
        bpl     :+

        jsr     get_win_horizscrollrect
        jsr     frame_winrect

:       lda     current_winfo::options
        and     #MGTK::option_grow_box
        beq     :+

        jsr     get_win_growboxrect
        jsr     frame_winrect
        jsr     get_win_vertscrollrect
        jsr     frame_winrect

:       jsr     get_window

        lda     current_winfo::id
        cmp     sel_window_id
        bne     :+

        jsr     set_desktop_port
        jmp     draw_winframe
:       rts
.endproc

        ;;  Drawing title bar, maybe?
draw_erase_mode:
        .byte   1

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
stripes_pattern:
stripes_pattern_alt := *+1
        .byte   %11111111
        .byte   %00000000
        .byte   %11111111
        .byte   %00000000
        .byte   %11111111
        .byte   %00000000
        .byte   %11111111
        .byte   %00000000
        .byte   %11111111

.proc set_stripes_pattern
        jsr     get_wingoaway_rect
        lda     winrect__y1
        and     #1
        beq     :+
        MGTK_CALL MGTK::SetPattern, stripes_pattern
        rts

:       MGTK_CALL MGTK::SetPattern, stripes_pattern_alt
        rts
.endproc

.else
stripes_pattern := 0
stripes_pattern_alt := 0
set_stripes_pattern := 0
.endif


.proc erase_winframe
        lda     #MGTK::penOR
        ldx     #0
        beq     :+
.endproc

.proc draw_winframe
        lda     #MGTK::penBIC
        ldx     #1

:       stx     draw_erase_mode
        jsr     set_fill_mode

        lda     current_winfo::options
        and     #MGTK::option_go_away_box
        beq     no_goaway

        lda     current_winfo::options
        and     #MGTK::option_dialog_box
        bne     no_goaway

        jsr     get_wingoaway_rect
        jsr     frame_winrect

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        jsr     set_stripes_pattern

        ldax    winrect__x1
        sec
        sbc     #9
        bcs     :+
        dex
:       stax    left

        clc
        adc     #6
        bcc     :+
        inx
:       stax    right

        copy16  winrect__y1, top
        copy16  winrect__y2, bottom

        jsr     PaintRectImpl  ; draws title bar stripes to left of close box

no_goaway:
        lda     current_winfo::options
        and     #MGTK::option_dialog_box
        bne     no_titlebar

        jsr     get_wintitlebar_rect
        jsr     center_title_text
        jsr     penloc_to_bounds
        jsr     set_stripes_pattern

        ldax    winrect__x2
        clc
        adc     #3
        bcc     :+
        inx
:       tay

        lda     current_winfo::options
        and     #MGTK::option_go_away_box
        bne     has_goaway

        tya
        sec
        sbc     #$1A
        bcs     :+
        dex
:       tay

has_goaway:
        tya
        ldy     right
        sty     winrect__x2
        ldy     right+1
        sty     winrect__x2+1

        ldy     left
        sty     right
        ldy     left+1
        sty     right+1

        stax    left

        lda     right
        sec
        sbc     #10
        sta     right
        bcs     :+
        dec     right+1

:       jsr     PaintRectImpl  ; Draw title bar stripes between close box and title
        add16   winrect__x2, #10, left

        jsr     get_wintitlebar_rect
        sub16   winrect__x2, #3, right

        jsr     PaintRectImpl  ; Draw title bar stripes to right of title
        MGTK_CALL MGTK::SetPattern, standard_port__penpattern

.else
has_goaway := 0

no_goaway:
        lda     #MGTK::penXOR
        jsr     set_fill_mode

        lda     current_winfo::options
        and     #MGTK::option_dialog_box
        bne     no_titlebar

        jsr     get_wintitlebar_rect
        jsr     center_title_text
        jsr     penloc_to_bounds

        lda     left
        sec
        sbc     #10
        sta     left
        bcs     :+
        dec     left+1
:
        lda     right
        clc
        adc     #10
        sta     right
        bcc     :+
        inc     right+1
:
        lda     top
        bne     :+
        dec     top+1
:       dec     top

        inc     bottom
        bne     :+
        inc     bottom+1
:
        jsr     PaintRectImpl
.endif

no_titlebar:
        jsr     get_window

        bit     current_winfo::vscroll
        bpl     no_vscroll

        jsr     get_win_vertscrollrect
        ldx     #3
:       lda     winrect,x
        sta     up_scroll_params,x
        sta     down_scroll_params,x
        dex
        bpl     :-

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        inc     up_scroll_params::ycoord
.endif

        ldax    winrect__y2
        sec
        sbc     #$0A
        bcs     :+
        dex
:
        pha
        lda     current_winfo::options
        and     #MGTK::option_grow_box
        bne     :+

        bit     current_winfo::hscroll
        bpl     no_hscroll

:       pla
        sec
        sbc     #$0B
        bcs     :+
        dex
:       pha

no_hscroll:
        pla
        stax    down_scroll_params::ycoord

        ldax    down_scroll_addr
        jsr     draw_icon

        ldax    up_scroll_addr
        jsr     draw_icon

no_vscroll:
        bit     current_winfo::hscroll
        bpl     no_hscrollbar

        jsr     get_win_horizscrollrect
        ldx     #3
:       lda     winrect,x
        sta     left_scroll_params,x
        sta     right_scroll_params,x
        dex
        bpl     :-

        ldax    winrect__x2
        sec
        sbc     #$14
        bcs     :+
        dex
:
        pha
        lda     current_winfo::options
        and     #MGTK::option_grow_box
        bne     :+

        bit     current_winfo::vscroll
        bpl     no_vscroll2

:       pla
        sec
        sbc     #$15
        bcs     :+
        dex
:       pha

no_vscroll2:
        pla
        stax    right_scroll_params

        ldax    right_scroll_addr
        jsr     draw_icon

        ldax    left_scroll_addr
        jsr     draw_icon

no_hscrollbar:
        lda     #MGTK::pencopy
        jsr     set_fill_mode

        lda     current_winfo::vscroll
        and     #$01
        beq     :+

        lda     #which_control_vert
        sta     which_control
        lda     draw_erase_mode
        jsr     draw_or_erase_scrollbar
        jsr     get_window

:       lda     current_winfo::hscroll
        and     #$01
        beq     :+

        lda     #which_control_horiz
        sta     which_control
        lda     draw_erase_mode
        jsr     draw_or_erase_scrollbar
        jsr     get_window

:       lda     current_winfo::options
        and     #MGTK::option_grow_box
        beq     ret

        jsr     get_win_growboxrect
        lda     draw_erase_mode
        bne     draw_resize
        ldax    #winrect
        jsr     fill_and_frame_rect
        jmp     ret

        ;; Draw resize box
draw_resize:
        ldx     #3
:       lda     winrect,x
        sta     resize_box_params,x
        dex
        bpl     :-

        lda     #MGTK::notpencopy
        jsr     set_fill_mode
        ldax    resize_box_addr
        jsr     draw_icon
ret:    rts
.endproc


.proc center_title_text
        ldax    current_winfo::title
        jsr     do_measure_text
        stax    $82

        lda     winrect__x1
        clc
        adc     winrect__x2
        tay

        lda     winrect__x1+1
        adc     winrect__x2+1
        tax

        tya
        sec
        sbc     $82
        tay

        txa
        sbc     $83
        cmp     #$80
        ror     a
        sta     current_penloc_x+1
        tya
        ror     a
        sta     current_penloc_x

        ldax    winrect__y2
        sec
        sbc     #2
        bcs     :+
        dex
:       stax    current_penloc_y

        ldax    $82
        rts
.endproc

;;; ============================================================

;;; 4 bytes of params, copied to current_penloc

.proc FindWindowImpl
        PARAM_BLOCK params, $EA
mousex:     .word   0
mousey:     .word   0
which_area: .byte   0
window_id:  .byte   0
        END_PARAM_BLOCK

        jsr     save_params_and_stack

        MGTK_CALL MGTK::InRect, test_rect_params ; check if in menubar
        beq     not_menubar

        lda     #MGTK::area_menubar
return_no_window:
        ldx     #0
return_result:
        pha
        txa
        pha
        jsr     restore_params_active_port
        pla
        tax
        pla
        ldy     #params::which_area - params
        jmp     store_xa_at_y

not_menubar:
        lda     #0              ; first window we see is the selected one
        sta     not_selected

        jsr     top_window
        beq     no_windows

loop:   jsr     get_winframerect
        jsr     in_winrect
        bne     in_window

        jsr     next_window
        stx     not_selected    ; set to non-zero for subsequent windows
        bne     loop

no_windows:
        lda     #MGTK::area_desktop
        beq     return_no_window

in_window:
        lda     current_winfo::options
        and     #MGTK::option_dialog_box
        bne     in_content

        jsr     get_wintitlebar_rect
        jsr     in_winrect
        beq     in_content

        lda     not_selected
        bne     :+

        lda     current_winfo::options
        and     #MGTK::option_go_away_box
        beq     :+

        jsr     get_wingoaway_rect
        jsr     in_winrect
        beq     :+
        lda     #MGTK::area_close_box
        bne     return_window

:       lda     #MGTK::area_dragbar
        bne     return_window

in_content:
        lda     not_selected
        bne     :+

        lda     current_winfo::options
        and     #MGTK::option_grow_box
        beq     :+

        jsr     get_win_growboxrect
        jsr     in_winrect
        beq     :+
        lda     #MGTK::area_grow_box
return_window:
        ldx     current_winfo::id
        bne     return_result

:       lda     #MGTK::area_content
        bne     return_window

not_selected:
        .byte   0

.endproc

;;; ============================================================
;;; OpenWindow

;;; params points to a winfo structure

.proc OpenWindowImpl
        win_id := $82

        copy16  params_addr, window

        ldy     #MGTK::winfo_offset_window_id
        lda     (window),y
        bne     :+
        exit_call MGTK__error_window_id_required

:       sta     win_id
        jsr     window_by_id
        beq     :+
        exit_call MGTK__error_window_already_exists

:       copy16  params_addr, window

        ldy     #MGTK::winfo_offset_status
        lda     (window),y
        ora     #$80
        sta     (window),y
        bmi     do_select_win
.endproc


;;; ============================================================
;;; SelectWindow

;;; 1 byte of params, copied to $82

.proc SelectWindowImpl
        param   := $82

        jsr     window_by_id_or_exit
        cmp     current_window
        bne     :+
        cpx     current_window+1
        bne     :+
        rts

:       jsr     link_window
do_select_win:
        ldy     #MGTK::winfo_offset_nextwinfo
        lda     current_window
        sta     (window),y
        iny
        lda     current_window+1
        sta     (window),y

        lda     window
        pha
        lda     window+1
        pha
        jsr     hide_cursor_save_params
        jsr     set_desktop_port

        jsr     top_window
        beq     :+
        jsr     erase_winframe
:       pla
        sta     current_window+1
        pla
        sta     current_window

        jsr     top_window
        lda     current_winfo::id
        sta     sel_window_id

        jsr     draw_window
        jmp     show_cursor_and_restore
.endproc

do_select_win   := SelectWindowImpl::do_select_win


.proc link_window
        ldy     #MGTK::winfo_offset_nextwinfo
        lda     (window),y
        sta     (previous_window),y
        iny
        lda     (window),y
        sta     (previous_window),y
        rts
.endproc


;;; ============================================================
;;; GetWinPtr

;;; 1 byte of params, copied to $82

.proc GetWinPtrImpl
        param := $82
        ptr := $A9

        jsr     window_by_id_or_exit
        ldax    ptr
        ldy     #1
        jmp     store_xa_at_y
.endproc

;;; ============================================================
;;; BeginUpdate

;;; 1 byte of params, copied to $82

previous_port:
        .res    2

update_port:
        .res    MGTK::grafport_size

.proc BeginUpdateImpl
        param   := $82

        jsr     window_by_id_or_exit

        lda     current_winfo::id
        cmp     target_window_id
        bne     :+
        inc     matched_target

:       jsr     hide_cursor_save_params
        jsr     set_desktop_port
        lda     matched_target
        bne     :+
        MGTK_CALL MGTK::SetPortBits, set_port_params

:       jsr     draw_window
        jsr     set_desktop_port
        lda     matched_target
        bne     :+
        MGTK_CALL MGTK::SetPortBits, set_port_params

:       jsr     get_window
        copy16  active_port, previous_port

        jsr     prepare_winport
        php
        ldax    update_port_addr
        jsr     assign_and_prepare_port

        asl     preserve_zp_flag
        plp
        bcc     :+
        rts
:       jsr     EndUpdateImpl
        ;; fall through
.endproc

err_obscured:
        exit_call MGTK__error_window_obscured

;;; ============================================================
;;; EndUpdate

;;; 1 byte of params, copied to $82

update_port_addr:
        .addr   update_port

.proc EndUpdateImpl
        param   := $82

        jsr     ShowCursorImpl

        ldax    previous_port
        stax    active_port
        jmp     set_and_prepare_port
.endproc

;;; ============================================================
;;; GetWinPort

;;; 3 bytes of params, copied to $82

.proc GetWinPortImpl
        PARAM_BLOCK params, $82
win_id:   .byte   0
win_port: .addr   0
        END_PARAM_BLOCK


        jsr     apply_port_to_active_port

        jsr     window_by_id_or_exit
        copy16  params::win_port, params_addr

        ldx     #7
:       lda     fill_rect_params,x
        sta     current_maprect_x1,x
        dex
        bpl     :-
        jsr     prepare_winport
        bcc     err_obscured

        ldy     #MGTK::grafport_size-1
:       lda     current_grafport,y
        sta     (params_addr),y
        dey
        bpl     :-

        jmp     apply_active_port_to_port
.endproc


.proc prepare_winport
        jsr     get_winrect

        ldx     #7
:       lda     #0
        sta     clipped_left,x
        lda     winrect,x
        sta     left,x
        dex
        bpl     :-

        jsr     clip_rect
        bcs     :+
        rts

        ;; Load window's grafport into current_grafport.
:       ldy     #MGTK::winfo_offset_port
:       lda     (window),y
        sta     current_grafport - MGTK::winfo_offset_port,y
        iny
        cpy     #MGTK::winfo_offset_port + MGTK::grafport_size
        bne     :-

        ldx     #2
:       lda     left,x
        sta     current_viewloc_x,x
        lda     left+1,x
        sta     current_viewloc_x+1,x
        lda     right,x
        sec
        sbc     left,x
        sta     $82,x
        lda     right+1,x
        sbc     left+1,x
        sta     $83,x

        lda     current_maprect_x1,x
        sec
        sbc     clipped_left,x
        sta     current_maprect_x1,x
        lda     current_maprect_x1+1,x
        sbc     clipped_left+1,x
        sta     current_maprect_x1+1,x

        lda     current_maprect_x1,x
        clc
        adc     $82,x
        sta     current_maprect_x2,x
        lda     current_maprect_x1+1,x
        adc     $83,x
        sta     current_maprect_x2+1,x

        dex
        dex
        bpl     :-
        sec
        rts
.endproc

;;; ============================================================
;;; SetWinPort

;;; 2 bytes of params, copied to $82

        ;; This updates win grafport from params ???
        ;; The math is weird; $82 is the window id so
        ;; how does ($82),y do anything useful - is
        ;; this buggy ???

        ;; It seems like it's trying to update a fraction
        ;; of the drawing port (from |pattern| to |font|)

.proc SetWinPortImpl
        params := $82
        ptr := window

        jsr     window_by_id_or_exit
        lda     ptr
        clc
        adc     #MGTK::winfo_offset_port
        sta     ptr
        bcc     :+
        inc     ptr+1

:       ldy     #MGTK::grafport_size-1
loop:   lda     ($82),y
        sta     (ptr),y
        dey
        cpy     #$10
        bcs     loop
        rts
.endproc

;;; ============================================================
;;; FrontWindow

.proc FrontWindowImpl
        jsr     top_window
        beq     nope
        lda     current_winfo::id
        bne     :+

nope:   lda     #0
:       ldy     #0
        sta     (params_addr),y
        rts
.endproc

;;; ============================================================
;;; TrackGoAway

in_close_box:  .byte   0

.proc TrackGoAwayImpl
        jsr     top_window
        beq     end

        jsr     get_wingoaway_rect
        jsr     save_params_and_stack
        jsr     set_desktop_port

        lda     #$80
toggle: sta     in_close_box

        lda     #MGTK::penXOR
        jsr     set_fill_mode

        jsr     HideCursorImpl
        MGTK_CALL MGTK::PaintRect, winrect
        jsr     ShowCursorImpl

loop:   jsr     get_and_return_event
        cmp     #MGTK::event_kind_button_up
        beq     :+

        MGTK_CALL MGTK::MoveTo, set_pos_params
        jsr     in_winrect
        eor     in_close_box
        bpl     loop
        lda     in_close_box
        eor     #$80
        jmp     toggle

:       jsr     restore_params_active_port
        ldy     #0
        lda     in_close_box
        beq     end
        lda     #1
end:    sta     (params_addr),y
        rts
.endproc

;;; ============================================================

        .byte   $00

.proc drag_initialpos
xcoord: .res    2
ycoord: .res    2
.endproc

.proc drag_curpos
xcoord: .res    2
ycoord: .res    2
.endproc

.proc drag_delta
xdelta: .res    2
ydelta: .res    2
.endproc

        ;; High bit set if window is being resized, clear if moved.
drag_resize_flag:
        .byte   0

;;; ============================================================

;;; 5 bytes of params, copied to $82

.proc GrowWindowImpl
        params := $82

        lda     #$80
        bmi     DragWindowImpl_drag_or_grow
.endproc

;;; ============================================================

;;; 5 bytes of params, copied to $82

.proc DragWindowImpl
        PARAM_BLOCK params, $82
window_id: .byte   0
dragx:     .word   0
dragy:     .word   0
moved:     .byte   0
        END_PARAM_BLOCK


        lda     #0
drag_or_grow:
        sta     drag_resize_flag
        jsr     kbd_mouse_init_tracking

        ldx     #3
:       lda     params::dragx,x
        sta     drag_initialpos,x
        sta     drag_curpos,x
        lda     #0
        sta     drag_delta,x
        dex
        bpl     :-

        jsr     window_by_id_or_exit

        bit     kbd_mouse_state
        bpl     :+
        jsr     kbd_win_drag_or_grow

:       jsr     hide_cursor_save_params
        jsr     winframe_to_set_port

        lda     #MGTK::penXOR
        jsr     set_fill_mode
        MGTK_CALL MGTK::SetPattern, checkerboard_pattern

loop:   jsr     get_window
        jsr     update_win_for_drag

        jsr     get_winframerect
        jsr     frame_winrect
        jsr     ShowCursorImpl

no_change:
        jsr     get_and_return_event
        cmp     #MGTK::event_kind_button_up
        bne     dragging

        jsr     frame_winrect

        bit     movement_cancel
        bmi     cancelled

        ldx     #3
:       lda     drag_delta,x
        bne     changed
        dex
        bpl     :-

cancelled:
        jsr     show_cursor_and_restore
        lda     #0
return_moved:
        ldy     #params::moved - params
        sta     (params_addr),y
        rts

changed:
        ldy     #MGTK::winfo_offset_port
:       lda     current_winport - MGTK::winfo_offset_port,y
        sta     (window),y
        iny
        cpy     #MGTK::winfo_offset_port + 16
        bne     :-
        jsr     HideCursorImpl

        lda     current_winfo::id
        jsr     erase_window
        jsr     hide_cursor_save_params

        bit     movement_cancel
        bvc     :+
        jsr     set_input

:       jsr     show_cursor_and_restore
        lda     #$80
        jmp     return_moved

dragging:
        jsr     check_if_changed
        beq     no_change

        jsr     HideCursorImpl
        jsr     frame_winrect
        jmp     loop
.endproc


.proc update_win_for_drag
        win_width := $82

        PARAM_BLOCK content, $C7
minwidth:  .word  0
minheight: .word  0
maxwidth:  .word  0
maxheight: .word  0
        END_PARAM_BLOCK


        ;; Copy mincontwidth..maxcontheight from the window to content
        ldy     #MGTK::winfo_offset_port-1
:       lda     (window),y
        sta     content - MGTK::winfo_offset_mincontwidth,y
        dey
        cpy     #MGTK::winfo_offset_mincontwidth-1
        bne     :-

        ldx     #0
        stx     force_tracking_change
        bit     drag_resize_flag
        bmi     grow

:       add16   current_winport::viewloc::xcoord,x, drag_delta,x, current_winport::viewloc::xcoord,x
        inx
        inx
        cpx     #4
        bne     :-

        lda     #$12
        cmp     current_winport::viewloc::ycoord
        bcc     :+
        sta     current_winport::viewloc::ycoord
:       rts

grow:   lda     #0
        sta     grew_flag
loop:   add16lc current_winport::maprect::x2,x, drag_delta,x, current_winport::maprect::x2,x
        sub16lc current_winport::maprect::x2,x, current_winport::maprect::x1,x, win_width

        sec
        lda     win_width
        sbc     content::minwidth,x
        lda     win_width+1
        sbc     content::minwidth+1,x
        bpl     :+

        add16lc content::minwidth,x, current_winport::maprect::x1,x, current_winport::maprect::x2,x
        jsr     set_grew_flag
        jmp     next

:       sec
        lda     content::maxwidth,x
        sbc     win_width
        lda     content::maxwidth+1,x
        sbc     win_width+1
        bpl     next

        add16lc content::maxwidth,x, current_winport::maprect::x1,x, current_winport::maprect::x2,x
        jsr     set_grew_flag

next:   inx
        inx
        cpx     #4
        bne     loop
        jmp     finish_grow
.endproc


        ;; Return with Z=1 if the drag position was not changed, or Z=0
        ;; if the drag position was changed or force_tracking_change is set.
.proc check_if_changed
        ldx     #2
        ldy     #0

loop:   lda     get_and_return_event__event__mouse_pos+1,x
        cmp     drag_curpos+1,x
        bne     :+
        iny
:
        lda     get_and_return_event__event__mouse_pos,x
        cmp     drag_curpos,x
        bne     :+
        iny
:       sta     drag_curpos,x

        sec
        sbc     drag_initialpos,x
        sta     drag_delta,x

        lda     get_and_return_event__event__mouse_pos+1,x
        sta     drag_curpos+1,x
        sbc     drag_initialpos+1,x
        sta     drag_delta+1,x

        dex
        dex
        bpl     loop

        cpy     #4
        bne     :+
        lda     force_tracking_change
:       rts
.endproc

DragWindowImpl_drag_or_grow := DragWindowImpl::drag_or_grow


;;; ============================================================
;;; CloseWindow

;;; 1 byte of params, copied to $82

.proc CloseWindowImpl
        param := $82

        jsr     window_by_id_or_exit

        jsr     hide_cursor_save_params

        jsr     winframe_to_set_port
        jsr     link_window

        ldy     #MGTK::winfo_offset_status
        lda     (window),y
        and     #$7F
        sta     (window),y

        jsr     top_window
        lda     current_winfo::id
        sta     sel_window_id
        lda     #0
        jmp     erase_window
.endproc

;;; ============================================================
;;; CloseAll

.proc CloseAllImpl
        jsr     top_window
        beq     :+

        ldy     #MGTK::winfo_offset_status
        lda     (window),y
        and     #$7F
        sta     (window),y
        jsr     link_window

        jmp     CloseAllImpl
:       jmp     StartDeskTopImpl__reset_desktop
.endproc


.proc winframe_to_set_port
        jsr     set_desktop_port
        jsr     get_winframerect

        ldx     #7
:       lda     winrect,x
        sta     left,x
        dex
        bpl     :-
        jsr     clip_rect

        ldx     #3
:       lda     left,x
        sta     set_port_maprect,x
        sta     set_port_params,x
        lda     right,x
        sta     set_port_size,x
        dex
        bpl     :-

        rts
.endproc


matched_target:
        .byte   0

        ;; Erases window after destruction
.proc erase_window
        sta     target_window_id
        lda     #0
        sta     matched_target
        MGTK_CALL MGTK::SetPortBits, set_port_params

        lda     #MGTK::pencopy
        jsr     set_fill_mode

        MGTK_CALL MGTK::SetPattern, checkerboard_pattern
        MGTK_CALL MGTK::PaintRect, set_port_maprect

        jsr     show_cursor_and_restore
        jsr     top_window
        beq     ret

        php
        sei
        jsr     FlushEventsImpl

:       jsr     next_window
        bne     :-

loop:   jsr     put_event
        bcs     plp_ret
        tax

        lda     #MGTK::event_kind_update
        sta     eventbuf__kind,x
        lda     current_winfo::id
        sta     eventbuf__window_id,x

        lda     current_winfo::id
        cmp     sel_window_id
        beq     plp_ret

        sta     $82
        jsr     window_by_id

        ldax    previous_window
        jsr     get_window::get_from_ax
        jmp     loop

plp_ret:
        plp
ret:    rts
.endproc


goaway_height:  .word   8       ; font height - 1
wintitle_height:.word  12       ; font height + 3
winframe_top:   .word  13       ; font height + 4

.proc set_port_params
left:           .word   0
top:            .word   $D
mapbits:        .addr   MGTK::screen_mapbits
mapwidth:       .word   MGTK::screen_mapwidth
hoffset:        .word   0
voffset:        .word   0
width:          .word   0
height:         .word   0
.endproc
        set_port_top  := set_port_params::top
        set_port_size := set_port_params::width
        set_port_maprect  := set_port_params::hoffset ; Re-used since h/voff are 0

;;; ============================================================
;;; WindowToScreen

        ;; $83/$84 += $B7/$B8
        ;; $85/$86 += $B9/$BA

.proc WindowToScreenImpl
        PARAM_BLOCK params, $82
window_id:      .byte   0
windowx:        .word   0
windowy:        .word   0
screenx:        .word   0       ; out
screeny:        .word   0       ; out
        END_PARAM_BLOCK

        jsr     window_by_id_or_exit

        ldx     #2
loop:   add16   params::windowx,x, current_winport::viewloc,x, params::windowx,x
        dex
        dex
        bpl     loop
        bmi     copy_map_results                  ; always
.endproc

;;; ============================================================
;;; ScreenToWindow

;;; 5 bytes of params, copied to $82

.proc ScreenToWindowImpl
        PARAM_BLOCK params, $82
window_id:      .byte   0
screenx:        .word   0
screeny:        .word   0
windowx:        .word   0       ; out
windowy:        .word   0       ; out
        END_PARAM_BLOCK

        jsr     window_by_id_or_exit

        ldx     #2
:       sub16   params::screenx,x, current_winport::viewloc,x, params::screenx,x
        dex
        dex
        bpl     :-
        ;; fall through
.endproc

.proc copy_map_results
        ldy     #ScreenToWindowImpl::params::windowx - ScreenToWindowImpl::params
:       lda     ScreenToWindowImpl::params + (ScreenToWindowImpl::params::screenx - ScreenToWindowImpl::params::windowx),y
        sta     (params_addr),y
        iny
        cpy     #ScreenToWindowImpl::params::size      ; results are 2 words (x, y) at params_addr+5
        bne     :-
        rts
.endproc

.if ::Variant <> 'P' && ::Variant <> 'M'

        ;; Window Manager API

        MGTK_DECL_API  OpenWindow,     $38, OpenWindowImpl, 0
        MGTK_DECL_API  CloseWindow,    $39, CloseWindowImpl, 0, ::param, 1
        MGTK_DECL_API  CloseAll,       $3A, CloseAllImpl, 0
        MGTK_DECL_API  GetWinPtr,      $3B, GetWinPtrImpl, 0, ::param, 1
        MGTK_DECL_API  GetWinPort,     $3C, GetWinPortImpl, 0, ::params
        MGTK_DECL_API  SetWinPort,     $3D, SetWinPortImpl, 0, ::params, 2
        MGTK_DECL_API  BeginUpdate,    $3E, BeginUpdateImpl, 0, ::param, 1
        MGTK_DECL_API  EndUpdate,      $3F, EndUpdateImpl, 0, ::param, 1
        MGTK_DECL_API  FindWindow,     $40, FindWindowImpl, 0, current_penloc, 4
        MGTK_DECL_API  FrontWindow,    $41, FrontWindowImpl, 0
        MGTK_DECL_API  SelectWindow,   $42, SelectWindowImpl, 0, ::param, 1
        MGTK_DECL_API  TrackGoAway,    $43, TrackGoAwayImpl, 0
        MGTK_DECL_API  DragWindow,     $44, DragWindowImpl, 0, ::params, 5
        MGTK_DECL_API  GrowWindow,     $45, GrowWindowImpl, 0, ::params, 5
        MGTK_DECL_API  ScreenToWindow, $46, ScreenToWindowImpl, 0, ::params, 5
        MGTK_DECL_API  WindowToScreen, $47, WindowToScreenImpl, 0, ::params, 5

.endif


        MGTK_DECL_ERROR  error_window_already_exists    ; $9D
        MGTK_DECL_ERROR  error_window_id_required       ; $9E
        MGTK_DECL_ERROR  error_window_not_found         ; $9F
        MGTK_DECL_ERROR  error_no_active_window         ; $A0
        MGTK_DECL_ERROR  error_window_not_draggable     ; $A1
        MGTK_DECL_ERROR  error_window_not_resizable     ; $A2
        MGTK_DECL_ERROR  error_window_obscured          ; $A3

        .include "windows-exp.inc"

