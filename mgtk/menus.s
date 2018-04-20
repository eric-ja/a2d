;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "apple2.inc"
        .include "../inc/apple2.inc"
        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        .import DrawTextImpl
        .import HideCursorImpl
        .import KeyboardMouseImpl
        .import SetPenModeImpl
        .import ShowCursorImpl
        .import adjust_xpos
        .import div7_table
        .import fixed_div
        .importzp fixed_div__dividend
        .importzp fixed_div__divisor
        .importzp fixed_div__quotient
        .import hide_cursor_save_params
        .import hires_table_hi
        .import hires_table_lo
        .import kbd_menu_return
        .import kbd_menu_select
        .import kbd_mouse_init_tracking
        .import kbd_mouse_state
        .import measure_text
        .importzp measure_text__data
        .importzp measure_text__length
        .import movement_cancel
        .import mult_x_y
        .import restore_params_active_port
        .import save_params_and_stack
        .import savebehind_size
        .import savebehind_usage
        .import sel_menu_index
        .import sel_menu_item_index
        .import set_pos_params__xcoord
        .import set_pos_params__ycoord
        .import set_standard_port
        .import show_cursor_and_restore
        .import standard_port__penpattern
        .import standard_port__textfont
        .import store_xa_at_params

        .import MGTK__FrameRect
        .import MGTK__GetEvent
        .import MGTK__InRect
        .import MGTK__PaintRect
        .import MGTK__SetPattern
        .import MGTK__MoveTo


;;; ============================================================

;;; Menu drawing metrics

offset_checkmark:   .byte   2
offset_text:        .byte   9
offset_shortcut:    .byte   16
shortcut_x_adj:     .byte   9
non_shortcut_x_adj: .byte   30
sysfont_height:     .byte   0


active_menu:
        .addr   0

.proc test_rect_params
left:   .word   $ffff
top:    .word   $ffff
right:  .word   $230
bottom: .word   $C
.endproc
        test_rect_top := test_rect_params::top
        test_rect_bottom := test_rect_params::bottom

.proc fill_rect_params2
left:   .word   0
top:    .word   0
width:  .word   0
height: .word   11
.endproc
        fill_rect_params2_height := fill_rect_params2::height

savebehind_buffer:
        .word   0

.proc test_rect_params2
left:   .word   0
top:    .word   12
right:  .word   0
bottom: .word   0
.endproc
        test_rect_params2_top := test_rect_params2::top

.proc fill_rect_params4
left:   .word   0
top:    .word   12
right:  .word   0
bottom: .word   0
.endproc
        fill_rect_params4_top := fill_rect_params4::top

menu_item_y_table:
        .repeat 15, i
        .byte   12 + 12 * i
        .endrepeat
menu_item_y_table_end:

solid_apple_glyph:
        .byte   $1E
open_apple_glyph:
        .byte   $1F
checkmark_glyph:
        .byte   $1D
controlkey_glyph:
        .byte   $01

shortcut_text:
        .byte   2              ; length
        .byte   $1E
        .byte   $FF

mark_text:
        .byte   1              ; length
        .byte   $1D

test_rect_params_addr:
        .addr   test_rect_params

test_rect_params2_addr:
        .addr   test_rect_params2

mark_text_addr:
        .addr   mark_text

shortcut_text_addr:
        .addr   shortcut_text


        menu_index        := $A7
        menu_count        := $A8
        menu_item_index   := $A9
        menu_item_count   := $AA
        menu_ptr          := $AB
        menu_item_ptr     := $AD


        PARAM_BLOCK curmenu, $AF
        ;; Public members
menu_id:    .byte  0
disabled:   .byte  0
title:      .addr  0
menu_items: .addr  0

        ;; Reserved area in menu
x_penloc:  .word   0
x_min:     .word   0
x_max:     .word   0
        END_PARAM_BLOCK


        PARAM_BLOCK curmenuinfo, $BB
        ;; Reserved area before first menu item
x_min:     .word   0
x_max:     .word   0
        END_PARAM_BLOCK


        PARAM_BLOCK curmenuitem, $BF
        ;; Public members
options:   .byte   0
mark_char: .byte   0
shortcut1: .byte   0
shortcut2: .byte   0
name:      .addr   0
        END_PARAM_BLOCK


.proc get_menu_count
        copy16  active_menu, $82
        ldy     #0
        lda     ($82),y
        sta     menu_count
        rts
.endproc


.proc get_menu
        stx     menu_index
        lda     #2
        clc
:       dex
        bmi     :+
        adc     #12
        bne     :-

:       adc     active_menu
        sta     menu_ptr
        lda     active_menu+1
        adc     #0
        sta     menu_ptr+1

        ldy     #MGTK::menu_size-1
:       lda     (menu_ptr),y
        sta     curmenu,y
        dey
        bpl     :-

        ldy     #MGTK::menu_item_size-1
:       lda     (curmenu::menu_items),y
        sta     curmenuinfo-1,y
        dey
        bne     :-

        lda     (curmenu::menu_items),y
        sta     menu_item_count
        rts
.endproc


.proc put_menu
        ldy     #MGTK::menu_size-1
:       lda     curmenu,y
        sta     (menu_ptr),y
        dey
        bpl     :-

        ldy     #MGTK::menu_item_size-1
:       lda     curmenuinfo-1,y
        sta     (curmenu::menu_items),y
        dey
        bne     :-
        rts
.endproc


.proc get_menu_item
        stx     menu_item_index
        lda     #MGTK::menu_item_size
        clc
:       dex
        bmi     :+
        adc     #MGTK::menu_item_size
        bne     :-
:
        adc     curmenu::menu_items
        sta     menu_item_ptr
        lda     curmenu::menu_items+1
        adc     #0
        sta     menu_item_ptr+1

        ldy     #MGTK::menu_item_size-1
:       lda     (menu_item_ptr),y
        sta     curmenuitem,y
        dey
        bpl     :-
        rts
.endproc


.proc put_menu_item
        ldy     #MGTK::menu_item_size-1
:       lda     curmenuitem,y
        sta     (menu_item_ptr),y
        dey
        bpl     :-
        rts
.endproc


        ;; Set penloc to X=AX, Y=Y
.proc set_penloc
        sty     current_penloc_y
        ldy     #0
        sty     current_penloc_y+1
set_x:  stax    current_penloc_x
        rts
.endproc

        ;; Set fill mode to A
.proc set_fill_mode
        sta     current_penmode
        jmp     SetPenModeImpl
.endproc

.proc do_measure_text
        jsr     prepare_text_params
        jmp     measure_text
.endproc

.proc draw_text
        jsr     prepare_text_params
        jmp     DrawTextImpl
.endproc

        ;; Prepare $A1,$A2 as params for TextWidth/DrawText call
        ;; ($A3 is length)
.proc prepare_text_params
        temp_ptr := $82

        stax    temp_ptr
        clc
        adc     #1
        bcc     :+
        inx
:       stax    measure_text__data
        ldy     #0
        lda     (temp_ptr),y
        sta     measure_text__length
        rts
.endproc

.proc get_and_return_event
        PARAM_BLOCK event, $82
kind:      .byte   0
mouse_pos:
mouse_x:   .word  0
mouse_y:   .word  0
        END_PARAM_BLOCK

        MGTK_CALL MGTK__GetEvent, event
        return  event
.endproc


;;; ============================================================
;;; SetMenu

need_savebehind:
        .res    2

.proc SetMenuImpl
        temp      := $82
        max_width := $C5

        lda     #0
        sta     savebehind_usage
        sta     savebehind_usage+1
        copy16  params_addr, active_menu

        jsr     get_menu_count  ; into menu_count
        jsr     hide_cursor_save_params
        jsr     set_standard_port

        ldax    test_rect_params_addr
        jsr     fill_and_frame_rect

        ldax    #12
        ldy     sysfont_height
        iny
        jsr     set_penloc

        ldx     #0
menuloop:
        jsr     get_menu
        ldax    current_penloc_x
        stax    curmenu::x_penloc

        sec
        sbc     #8
        bcs     :+
        dex
:       stax    curmenu::x_min
        stax    curmenuinfo::x_min

        ldx     #0
        stx     max_width
        stx     max_width+1

itemloop:
        jsr     get_menu_item
        bit     curmenuitem::options
        bvs     filler                  ; bit 6 - is filler

        ldax    curmenuitem::name
        jsr     do_measure_text
        stax    temp

        lda     curmenuitem::options
        and     #3                      ; OA+SA
        bne     :+
        lda     curmenuitem::shortcut1
        bne     :+
        lda     shortcut_x_adj
        bne     has_shortcut

:       lda     non_shortcut_x_adj
has_shortcut:
        clc
        adc     temp
        sta     temp
        bcc     :+
        inc     temp+1
:
        sec
        sbc     max_width
        lda     temp+1
        sbc     max_width+1
        bmi     :+
        copy16  temp, max_width          ; calculate max width
:
filler: ldx     menu_item_index
        inx
        cpx     menu_item_count
        bne     itemloop

        lda     menu_item_count
        tax
        ldy     sysfont_height
        iny
        iny
        iny
        jsr     mult_x_y                ; num items * (sysfont_height+3)
        pha

        copy16  max_width, fixed_div__dividend
        copy16  #7, fixed_div__divisor
        jsr     fixed_div               ; max width / 7

        ldy     fixed_div__quotient+2
        iny
        iny
        pla
        tax
        jsr     mult_x_y                ; total height * ((max width / 7)+2)

        sta     need_savebehind
        sty     need_savebehind+1
        sec
        sbc     savebehind_usage
        tya
        sbc     savebehind_usage+1
        bmi     :+
        copy16  need_savebehind, savebehind_usage     ; calculate max savebehind data needed

:       add16_8 curmenuinfo::x_min, max_width, curmenuinfo::x_max

        jsr     put_menu

        ldax    curmenu::title
        jsr     draw_text
        jsr     get_menu_and_menu_item

        ldax    current_penloc_x
        clc
        adc     #8
        bcc     :+
        inx
:       stax    curmenu::x_max

        jsr     put_menu

        ldax    #12
        jsr     adjust_xpos

        ldx     menu_index
        inx
        cpx     menu_count
        beq     :+
        jmp     menuloop

:       lda     #0
        sta     sel_menu_index
        sta     sel_menu_item_index

        jsr     show_cursor_and_restore
        sec
        lda     savebehind_size
        sbc     savebehind_usage
        lda     savebehind_size+1
        sbc     savebehind_usage+1
        bpl     :+
        exit_call MGTK::error_insufficient_savebehind_area

:       rts
.endproc


.proc get_menu_and_menu_item
        ldx     menu_index
        jsr     get_menu

        ldx     menu_item_index
        jmp     get_menu_item
.endproc


        ;; Fills rect (params at X,A) then inverts border
.proc fill_and_frame_rect
        stax    fill_params
        stax    draw_params
        lda     #MGTK::pencopy
        jsr     set_fill_mode
        MGTK_CALL MGTK__PaintRect, 0, fill_params
        lda     #MGTK::notpencopy
        jsr     set_fill_mode
        MGTK_CALL MGTK__FrameRect, 0, draw_params
        rts
.endproc


.proc find_menu_by_id_or_fail
        jsr     find_menu_by_id
        bne     :+
        exit_call MGTK::error_menu_not_found
:       rts
.endproc


        find_mode             := $C6

        find_mode_by_id       := $00        ; find menu/menu item by id
        find_menu_id          := $C7
        find_menu_item_id     := $C8

        find_mode_by_coord    := $80        ; find menu by x-coord/menu item by y-coord
                                            ; coordinate is in set_pos_params

        find_mode_by_shortcut := $C0        ; find menu and menu item by shortcut key
        find_shortcut         := $C9
        find_options          := $CA


.proc find_menu_by_id
        lda     #find_mode_by_id
find_menu:
        sta     find_mode

        jsr     get_menu_count
        ldx     #0
loop:   jsr     get_menu
        bit     find_mode
        bvs     find_menu_item_mode
        bmi     :+

        lda     curmenu::menu_id          ; search by menu id
        cmp     find_menu_id
        bne     next
        beq     found

:       ldax    set_pos_params__xcoord    ; search by x coordinate bounds
        cpx     curmenu::x_min+1
        bcc     next
        bne     :+
        cmp     curmenu::x_min
        bcc     next
:       cpx     curmenu::x_max+1
        bcc     found
        bne     next
        cmp     curmenu::x_max
        bcc     found
        bcs     next

find_menu_item_mode:
        jsr     find_menu_item
        bne     found

next:   ldx     menu_index
        inx
        cpx     menu_count
        bne     loop
        return  #0

found:  return  curmenu::menu_id
.endproc

find_menu := find_menu_by_id::find_menu


.proc find_menu_item
        ldx     #0
loop:   jsr     get_menu_item
        ldx     menu_item_index
        inx
        bit     find_mode
        bvs     find_by_shortcut
        bmi     :+

        cpx     find_menu_item_id
        bne     next
        beq     found

:       lda     menu_item_y_table,x
        cmp     set_pos_params__ycoord
        bcs     found
        bcc     next

find_by_shortcut:
        lda     find_shortcut
        and     #$7F
        cmp     curmenuitem::shortcut1
        beq     :+
        cmp     curmenuitem::shortcut2
        bne     next

:       cmp     #$20             ; is control char
        bcc     found
        lda     curmenuitem::options
        and     #MGTK::menuopt_disable_flag | MGTK::menuopt_item_is_filler
        bne     next

        lda     curmenuitem::options
        and     find_options
        bne     found

next:   cpx     menu_item_count
        bne     loop
        ldx     #0
found:  rts
.endproc


;;; ============================================================
;;; HiliteMenu

;;; 2 bytes of params, copied to $C7

.proc HiliteMenuImpl
        menu_param := $C7

        lda     menu_param
        bne     :+
        lda     cur_open_menu
        sta     menu_param

:       jsr     find_menu_by_id_or_fail

do_hilite:
        jsr     hide_cursor_save_params
        jsr     set_standard_port
        jsr     hilite_menu
        jmp     show_cursor_and_restore
.endproc

        ;; Highlight/Unhighlight top level menu item
.proc hilite_menu
        ldx     #1
loop:   lda     curmenu::x_min,x
        sta     fill_rect_params2::left,x
        lda     curmenu::x_max,x
        sta     fill_rect_params2::width,x

        lda     curmenuinfo::x_min,x
        sta     test_rect_params2::left,x
        sta     fill_rect_params4::left,x

        lda     curmenuinfo::x_max,x
        sta     test_rect_params2::right,x
        sta     fill_rect_params4::right,x

        dex
        bpl     loop

        lda     #MGTK::penXOR
        jsr     set_fill_mode
        MGTK_CALL MGTK__PaintRect, fill_rect_params2
        rts
.endproc

;;; ============================================================
;;; MenuKey

;;; 4 bytes of params, copied to $C7

.proc MenuKeyImpl
        PARAM_BLOCK params, $C7
menu_id:   .byte   0
menu_item: .byte   0
which_key: .byte   0
key_mods:  .byte   0
        END_PARAM_BLOCK


        lda     params::which_key
        cmp     #$1B                     ; escape key
        bne     :+

        lda     params::key_mods
        bne     :+
        jsr     KeyboardMouseImpl
        jmp     MenuSelectImpl


:       lda     #find_mode_by_shortcut
        jsr     find_menu
        beq     not_found

        lda     curmenu::disabled
        bmi     not_found

        lda     curmenuitem::options
        and     #MGTK::menuopt_disable_flag | MGTK::menuopt_item_is_filler
        bne     not_found

        lda     curmenu::menu_id
        sta     cur_open_menu
        bne     found

not_found:
        lda     #0
        tax
found:  ldy     #0
        sta     (params_addr),y
        iny
        txa
        sta     (params_addr),y
        bne     HiliteMenuImpl::do_hilite
        rts
.endproc


.proc find_menu_and_menu_item
        jsr     find_menu_by_id_or_fail
        jsr     find_menu_item
        cpx     #0
.endproc
rrts:   rts

.proc find_menu_item_or_fail
        jsr     find_menu_and_menu_item
        bne     rrts
        exit_call MGTK::error_menu_item_not_found
.endproc


;;; ============================================================
;;; DisableItem

;;; 3 bytes of params, copied to $C7

.proc DisableItemImpl
        PARAM_BLOCK params, $C7
menu_id:   .byte   0
menu_item: .byte   0
disable:   .byte   0
        END_PARAM_BLOCK


        jsr     find_menu_item_or_fail

        asl     curmenuitem::options
        ror     params::disable
        ror     curmenuitem::options

        jmp     put_menu_item
.endproc

;;; ============================================================
;;; CheckItem

;;; 3 bytes of params, copied to $C7

.proc CheckItemImpl
        PARAM_BLOCK params, $C7
menu_id:   .byte   0
menu_item: .byte   0
check:     .byte   0
        END_PARAM_BLOCK


        jsr     find_menu_item_or_fail

        lda     params::check
        beq     :+
        lda     #MGTK::menuopt_item_is_checked
        ora     curmenuitem::options
        bne     set_options            ; always

:       lda     #$FF^MGTK::menuopt_item_is_checked
        and     curmenuitem::options
set_options:
        sta     curmenuitem::options
        jmp     put_menu_item
.endproc

;;; ============================================================
;;; DisableMenu

;;; 2 bytes of params, copied to $C7

.proc DisableMenuImpl
        PARAM_BLOCK params, $C7
menu_id:   .byte   0
disable:   .byte   0
        END_PARAM_BLOCK


        jsr     find_menu_by_id_or_fail

        asl     curmenu::disabled
        ror     params::disable
        ror     curmenu::disabled

        ldx     menu_index
        jmp     put_menu
.endproc

;;; ============================================================
;;; MenuSelect

cur_open_menu:
        .byte   0

cur_hilited_menu_item:
        .byte   0

.proc MenuSelectImpl
        PARAM_BLOCK params, $C7
menu_id:   .byte   0
menu_item: .byte   0
        END_PARAM_BLOCK


        jsr     kbd_mouse_init_tracking

        jsr     get_menu_count
        jsr     save_params_and_stack
        jsr     set_standard_port

        bit     kbd_mouse_state
        bpl     :+
        jsr     kbd_menu_select
        jmp     in_menu

:       lda     #0
        sta     cur_open_menu
        sta     cur_hilited_menu_item
        jsr     get_and_return_event
event_loop:
        bit     movement_cancel
        bpl     :+
        jmp     kbd_menu_return

:       MGTK_CALL MGTK__MoveTo, get_and_return_event::event::mouse_pos
        MGTK_CALL MGTK__InRect, test_rect_params      ; test in menu bar
        bne     in_menu_bar
        lda     cur_open_menu
        beq     in_menu

        MGTK_CALL MGTK__InRect, test_rect_params2     ; test in menu
        bne     in_menu_item
        jsr     unhilite_cur_menu_item

in_menu:jsr     get_and_return_event
        beq     :+
        cmp     #MGTK::event_kind_button_up
        bne     event_loop

:       lda     cur_hilited_menu_item
        bne     :+
        jsr     hide_menu
        jmp     restore

:       jsr     HideCursorImpl
        jsr     set_standard_port
        jsr     restore_menu_savebehind

restore:jsr     restore_params_active_port
        lda     #0

        ldx     cur_hilited_menu_item
        beq     :+

        lda     cur_open_menu
        ldy     menu_index             ; ???
        sty     sel_menu_index
        stx     sel_menu_item_index

:       jmp     store_xa_at_params


in_menu_bar:
        jsr     unhilite_cur_menu_item

        lda     #find_mode_by_coord
        jsr     find_menu

        cmp     cur_open_menu
        beq     in_menu
        pha
        jsr     hide_menu
        pla
        sta     cur_open_menu

        jsr     draw_menu
        jmp     in_menu


in_menu_item:
        lda     #find_mode_by_coord
        sta     find_mode
        jsr     find_menu_item
        cpx     cur_hilited_menu_item
        beq     in_menu

        lda     curmenu::disabled
        ora     curmenuitem::options
        and     #MGTK::menuopt_disable_flag | MGTK::menuopt_item_is_filler
        beq     :+

        ldx     #0
:       txa
        pha
        jsr     hilite_menu_item
        pla
        sta     cur_hilited_menu_item
        jsr     hilite_menu_item

        jmp     in_menu
.endproc


        savebehind_left_bytes := $82
        savebehind_bottom := $83

        savebehind_buf_addr := $8E
        savebehind_vid_addr := $84
        savebehind_mapwidth := $90


.proc set_up_savebehind
        lda     curmenuinfo::x_min+1
        lsr     a
        lda     curmenuinfo::x_min
        ror     a
        tax
        lda     div7_table,x
        sta     savebehind_left_bytes

        lda     curmenuinfo::x_max+1
        lsr     a
        lda     curmenuinfo::x_max
        ror     a
        tax
        lda     div7_table,x
        sec
        sbc     savebehind_left_bytes
        sta     savebehind_mapwidth

        copy16  savebehind_buffer, savebehind_buf_addr

        ldy     menu_item_count
        ldx     menu_item_y_table,y ; ???
        inx
        stx     savebehind_bottom
        stx     fill_rect_params4::bottom
        stx     test_rect_params2::bottom

        ldx     sysfont_height
        inx
        inx
        inx
        stx     fill_rect_params4::top
        stx     test_rect_params2::top
        rts
.endproc


.proc savebehind_get_vidaddr
        lda     hires_table_lo,x
        clc
        adc     savebehind_left_bytes
        sta     savebehind_vid_addr
        lda     hires_table_hi,x
        ora     #$20
        sta     savebehind_vid_addr+1
        rts
.endproc


.proc savebehind_next_line
        lda     savebehind_buf_addr
        sec
        adc     savebehind_mapwidth
        sta     savebehind_buf_addr
        bcc     :+
        inc     savebehind_buf_addr+1
:       rts
.endproc


.proc restore_menu_savebehind
        jsr     set_up_savebehind
loop:   jsr     savebehind_get_vidaddr
        sta     HISCR

        ldy     savebehind_mapwidth
:       lda     (savebehind_buf_addr),y
        sta     (savebehind_vid_addr),y
        dey
        bpl     :-
        jsr     savebehind_next_line
        sta     LOWSCR

        ldy     savebehind_mapwidth
:       lda     (savebehind_buf_addr),y
        sta     (savebehind_vid_addr),y
        dey
        bpl     :-
        jsr     savebehind_next_line

        inx
        cpx     savebehind_bottom
        bcc     loop
        beq     loop
        jmp     ShowCursorImpl
.endproc


dmrts:  rts


.proc hide_menu
        clc
        bcc     draw_menu_draw_or_hide
.endproc


.proc draw_menu
        sec
draw_or_hide:
        lda     cur_open_menu
        beq     dmrts
        php

        sta     find_menu_id
        jsr     find_menu_by_id
        jsr     HideCursorImpl
        jsr     hilite_menu

        plp
        bcc     restore_menu_savebehind

        jsr     set_up_savebehind
saveloop:
        jsr     savebehind_get_vidaddr
        sta     HISCR

        ldy     savebehind_mapwidth
:       lda     (savebehind_vid_addr),y
        sta     (savebehind_buf_addr),y
        dey
        bpl     :-
        jsr     savebehind_next_line
        sta     LOWSCR

        ldy     savebehind_mapwidth
:       lda     (savebehind_vid_addr),y
        sta     (savebehind_buf_addr),y
        dey
        bpl     :-
        jsr     savebehind_next_line

        inx
        cpx     savebehind_bottom
        bcc     saveloop
        beq     saveloop

        jsr     set_standard_port

        ldax    test_rect_params2_addr
        jsr     fill_and_frame_rect
        inc16   fill_rect_params4::left
        lda     fill_rect_params4::right
        bne     :+
        dec     fill_rect_params4::right+1
:       dec     fill_rect_params4::right

        jsr     get_menu_and_menu_item

        ldx     #0
loop:   jsr     get_menu_item
        bit     curmenuitem::options
        bvc     :+
        jmp     next

:       lda     curmenuitem::options
        and     #MGTK::menuopt_item_is_checked
        beq     no_mark

        lda     offset_checkmark
        jsr     moveto_menuitem

        lda     checkmark_glyph
        sta     mark_text+1

        lda     curmenuitem::options
        and     #MGTK::menuopt_item_has_mark
        beq     :+
        lda     curmenuitem::mark_char
        sta     mark_text+1

:       ldax    mark_text_addr
        jsr     draw_text
        jsr     get_menu_and_menu_item

no_mark:
        lda     offset_text
        jsr     moveto_menuitem

        ldax    curmenuitem::name
        jsr     draw_text

        jsr     get_menu_and_menu_item
        lda     curmenuitem::options
        and     #MGTK::menuopt_open_apple | MGTK::menuopt_solid_apple
        bne     oa_sa

        lda     curmenuitem::shortcut1
        beq     no_shortcut

        lda     controlkey_glyph
        sta     shortcut_text+1
        jmp     no_shortcut

oa_sa:  cmp     #MGTK::menuopt_open_apple
        bne     :+
        lda     open_apple_glyph
        sta     shortcut_text+1
        jmp     shortcut

:       lda     solid_apple_glyph
        sta     shortcut_text+1

shortcut:
        lda     curmenuitem::shortcut1
        sta     shortcut_text+2

        lda     offset_shortcut
        jsr     moveto_fromright

        ldax    shortcut_text_addr
        jsr     draw_text
        jsr     get_menu_and_menu_item

no_shortcut:
        bit     curmenu::disabled
        bmi     :+
        bit     curmenuitem::options
        bpl     next

:       jsr     dim_menuitem
        jmp     next                   ; useless jmp ???

next:   ldx     menu_item_index
        inx
        cpx     menu_item_count
        beq     :+
        jmp     loop
:       jmp     ShowCursorImpl
.endproc


.proc moveto_menuitem
        ldx     menu_item_index
        ldy     menu_item_y_table+1,x ; ???
        dey
        ldx     curmenuinfo::x_min+1
        clc
        adc     curmenuinfo::x_min
        bcc     :+
        inx
:       jmp     set_penloc
.endproc


.proc dim_menuitem
        ldx     menu_item_index
        lda     menu_item_y_table,x
        sta     fill_rect_params3_top
        inc     fill_rect_params3_top
        lda     menu_item_y_table+1,x
        sta     fill_rect_params3_bottom

        add16lc curmenuinfo::x_min, #5, fill_rect_params3_left
        sub16lc curmenuinfo::x_max, #5, fill_rect_params3_right

        MGTK_CALL MGTK__SetPattern, light_speckle_pattern

        lda     #MGTK::penOR
        jsr     set_fill_mode

        MGTK_CALL MGTK__PaintRect, fill_rect_params3
        MGTK_CALL MGTK__SetPattern, standard_port__penpattern

        lda     #MGTK::penXOR
        jsr     set_fill_mode
        rts
.endproc

draw_menu_draw_or_hide := draw_menu::draw_or_hide


light_speckle_pattern:
        .byte   %10001000
        .byte   %01010101
        .byte   %10001000
        .byte   %01010101
        .byte   %10001000
        .byte   %01010101
        .byte   %10001000
        .byte   %01010101

.proc fill_rect_params3
left:   .word   0
top:    .word   0
right:  .word   0
bottom: .word   0
.endproc
        fill_rect_params3_left := fill_rect_params3::left
        fill_rect_params3_top := fill_rect_params3::top
        fill_rect_params3_right := fill_rect_params3::right
        fill_rect_params3_bottom := fill_rect_params3::bottom


        ;; Move to the given distance from the right side of the menu.
.proc moveto_fromright
        sta     $82
        ldax    curmenuinfo::x_max
        sec
        sbc     $82
        bcs     :+
        dex
:       jmp     set_penloc::set_x
.endproc

.proc unhilite_cur_menu_item
        jsr     hilite_menu_item
        lda     #0
        sta     cur_hilited_menu_item
.endproc
hmrts:  rts

.proc hilite_menu_item
        ldx     cur_hilited_menu_item
        beq     hmrts
        ldy     menu_item_y_table-1,x
        iny
        sty     fill_rect_params4::top
        ldy     menu_item_y_table,x
        sty     fill_rect_params4::bottom
        jsr     HideCursorImpl

        lda     #MGTK::penXOR
        jsr     set_fill_mode
        MGTK_CALL MGTK__PaintRect, fill_rect_params4
        jmp     ShowCursorImpl
.endproc

;;; ============================================================
;;; InitMenu

;;; 4 bytes of params, copied to $82

.proc InitMenuImpl
        params := $82

        ldx     #3
loop:   lda     params,x
        sta     solid_apple_glyph,x
        dex
        bpl     loop

        copy16  standard_port__textfont, params
        ldy     #0
        lda     (params),y
        bmi     :+                    ; branch if double-width font

        lda     #2
        sta     offset_checkmark
        lda     #9
        sta     offset_text
        lda     #16
        sta     offset_shortcut
        lda     #9
        sta     shortcut_x_adj
        lda     #30
        sta     non_shortcut_x_adj
        bne     end

:       lda     #2
        sta     offset_checkmark
        lda     #16
        sta     offset_text
        lda     #30
        sta     offset_shortcut
        lda     #16
        sta     shortcut_x_adj
        lda     #51
        sta     non_shortcut_x_adj

end:
.if ::VStatus < 'B' || (::VStatus = 'B' && ::VRelease < 10)
        ldy     #2
        lda     (params),y
        tay
        iny
        iny
        iny

        ldx     #0
:       tya
        adc     menu_item_y_table,x
        inx
        sta     menu_item_y_table,x
        cpx     #14
        bcc     :-
.endif
        rts
.endproc

;;; ============================================================
;;; SetMark

;;; 4 bytes of params, copied to $C7

.proc SetMarkImpl
        PARAM_BLOCK params, $C7
menu_id:   .byte   0
menu_item: .byte   0
set_char:  .byte   0
mark_char: .byte   0
        END_PARAM_BLOCK


        jsr     find_menu_item_or_fail

        lda     params::set_char
        beq     :+

        lda     #MGTK::menuopt_item_has_mark
        ora     curmenuitem::options
        sta     curmenuitem::options

        lda     params::mark_char
        sta     curmenuitem::mark_char
        jmp     put_menu_item

:       lda     #$FF^MGTK::menuopt_item_has_mark
        and     curmenuitem::options
        sta     curmenuitem::options
        jmp     put_menu_item
.endproc


        ;; Menu Manager API

        MGTK_DECL_API  InitMenu,    $2F, InitMenuImpl, 0, ::params, 4
        MGTK_DECL_API  SetMenu,     $30, SetMenuImpl, 0
        MGTK_DECL_API  MenuSelect,  $31, MenuSelectImpl, 0
        MGTK_DECL_API  MenuKey,     $32, MenuKeyImpl, 0, ::params
        MGTK_DECL_API  HiliteMenu,  $33, HiliteMenuImpl, 0, ::menu_param, 1
        MGTK_DECL_API  DisableMenu, $34, DisableMenuImpl, 0, ::params
        MGTK_DECL_API  DisableItem, $35, DisableItemImpl, 0, ::params
        MGTK_DECL_API  CheckItem,   $36, CheckItemImpl, 0, ::params
        MGTK_DECL_API  SetMark,     $37, SetMarkImpl, 0, ::params


        MGTK_DECL_ERROR  error_menu_not_found                 ; $9A
        MGTK_DECL_ERROR  error_menu_item_not_found            ; $9B
        MGTK_DECL_ERROR  error_insufficient_savebehind_area   ; $9C


        .include "menus-exp.inc"
