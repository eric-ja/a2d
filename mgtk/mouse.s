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


        MGTK_IMPORT HiliteMenuImpl
        MGTK_IMPORTZP MenuSelectImpl__params__menu_id
        MGTK_IMPORTZP MenuSelectImpl__params__menu_item
        MGTK_IMPORT SetCursorImpl

        MGTK_IMPORTZP curmenu__disabled
        MGTK_IMPORTZP curmenu__menu_id
        MGTK_IMPORTZP curmenu__x_min
        MGTK_IMPORTZP curmenuitem__options
.if ::Variant <> 'M'
        MGTK_IMPORTZP current_winfo__options
.endif
        MGTK_IMPORTZP find_menu_id
        MGTK_IMPORTZP find_options
        MGTK_IMPORTZP find_shortcut
        MGTK_IMPORTZP winrect
        MGTK_IMPORTZP winrect__x1
        MGTK_IMPORTZP winrect__y1
        MGTK_IMPORTZP winrect__x2
        MGTK_IMPORTZP winrect__y2

        MGTK_IMPORT active_cursor
        MGTK_IMPORT call_mouse
        MGTK_IMPORT cur_hilited_menu_item
        MGTK_IMPORT cur_open_menu
.if ::Variant <> 'M'
        MGTK_IMPORT drag_curpos
        MGTK_IMPORT drag_initialpos
        MGTK_IMPORT drag_initialpos__xcoord
        MGTK_IMPORT drag_initialpos__ycoord
        MGTK_IMPORT drag_resize_flag
.endif
        MGTK_IMPORT draw_menu
        MGTK_IMPORT find_menu
        MGTK_IMPORTZP find_mode_by_shortcut
        MGTK_IMPORT get_menu
        MGTK_IMPORT get_menu_item
.if ::Variant <> 'M'
        MGTK_IMPORT get_winframerect
.endif
        MGTK_IMPORT hide_menu
        MGTK_IMPORT hilite_menu_item
        MGTK_IMPORT input__modifiers
        MGTK_IMPORTZP menu_count
        MGTK_IMPORTZP menu_item_count
        MGTK_IMPORT menu_item_y_table
        MGTK_IMPORT mouse_hooked_flag
        MGTK_IMPORT mouse_scale_x
        MGTK_IMPORT mouse_scale_y
        MGTK_IMPORT mouse_status
        MGTK_IMPORT mouse_x
        MGTK_IMPORT mouse_y
        MGTK_IMPORT pointer_cursor_addr
        MGTK_IMPORT read_mouse_pos
        MGTK_IMPORT restore_params_active_port
        MGTK_IMPORTZP screen_height
        MGTK_IMPORT screen_width
        MGTK_IMPORT set_pos_params
        MGTK_IMPORT store_xa_at_params


;;; ============================================================

        ;; Set to $80 by KeyboardMouse call; also set to $04,
        ;; $01 elsewhere.
kbd_mouse_state:
        .byte   0

kbd_mouse_state_menu := 1
kbd_mouse_state_mousekeys := 4


kbd_mouse_x:  .word     0
kbd_mouse_y:  .word     0

kbd_menu_select_flag:
        .byte   0

        ;; Currently selected menu/menu item. Note that menu is index,
        ;; not ID from menu definition.
sel_menu_index:
        .byte   0
sel_menu_item_index:
        .byte   0

saved_mouse_pos:
saved_mouse_x:  .word   0
saved_mouse_y:  .byte   0

kbd_menu:  .byte   $00
kbd_menu_item:  .byte   $00
movement_cancel:  .byte   $00
kbd_mouse_status:  .byte   $00

.proc kbd_mouse_save_zp
        ldx     #$7F
:       lda     $80,x
        sta     kbd_mouse_zp_stash,x
        dex
        bpl     :-
        rts
.endproc


.proc kbd_mouse_restore_zp
        ldx     #$7F
:       lda     kbd_mouse_zp_stash,x
        sta     $80,x
        dex
        bpl     :-
        rts
.endproc


kbd_mouse_zp_stash:
        .res    128


;;; ============================================================
;;; X = xlo, Y = xhi, A = y


.proc set_mouse_pos
        bit     mouse_hooked_flag
        bmi     no_firmware
        bit     no_mouse_flag
        bmi     no_firmware

        pha
        txa
        sec
        jsr     scale_mouse_coord

        ldx     mouse_firmware_hi
        sta     MOUSE_X_LO,x
        tya
        sta     MOUSE_X_HI,x

        pla
        ldy     #$00
        clc
        jsr     scale_mouse_coord

        ldx     mouse_firmware_hi
        sta     MOUSE_Y_LO,x
        tya
        sta     MOUSE_Y_HI,x

        ldy     #POSMOUSE
        jmp     call_mouse

no_firmware:
        stx     mouse_x
        sty     mouse_x+1
        sta     mouse_y
        bit     mouse_hooked_flag
        bpl     not_hooked
        ldy     #POSMOUSE
        jmp     call_mouse

not_hooked:
        rts
.endproc

;;; ============================================================

.proc restore_mouse_pos
        ldx     saved_mouse_x
        ldy     saved_mouse_x+1
        lda     saved_mouse_y
        jmp     set_mouse_pos
.endproc

.proc set_mouse_pos_from_kbd_mouse
        ldx     kbd_mouse_x
        ldy     kbd_mouse_x+1
        lda     kbd_mouse_y
        jmp     set_mouse_pos
.endproc


.proc scale_mouse_coord
        bcc     scale_y
        ldx     mouse_scale_x
        bne     :+
ret:    rts

scale_y:
        ldx     mouse_scale_y
        beq     ret

:       pha
        tya
        lsr     a
        tay
        pla
        ror     a
        dex
        bne     :-
        rts
.endproc


.proc kbd_mouse_to_mouse
        ldx     #2
:       lda     kbd_mouse_x,x
        sta     mouse_x,x
        dex
        bpl     :-
        rts
.endproc

.proc position_kbd_mouse
        jsr     kbd_mouse_to_mouse
        jmp     set_mouse_pos_from_kbd_mouse
.endproc


.proc save_mouse_pos
        jsr     read_mouse_pos
        ldx     #2
:       lda     mouse_x,x
        sta     saved_mouse_pos,x
        dex
        bpl     :-
        rts
.endproc

.proc restore_cursor
        jsr     stash_addr
        copy16  kbd_mouse_cursor_stash, params_addr
        jsr     SetCursorImpl
        jsr     restore_addr

        lda     #0
        sta     kbd_mouse_state
        lda     #$40
        sta     mouse_status
        jmp     restore_mouse_pos
.endproc

.proc kbd_mouse_init_tracking
        lda     #0
        sta     movement_cancel
        sta     force_tracking_change
        rts
.endproc

        ;; Look at buttons (apple keys), compute modifiers in A
        ;; (bit 0 = button 0 / open apple, bit 1 = button 1 / solid apple)
.proc compute_modifiers
        lda     BUTN1
        asl     a
        lda     BUTN0
        and     #$80
        rol     a
        rol     a
        rts
.endproc


.proc get_key
        jsr     compute_modifiers
        sta     set_input_modifiers
no_modifiers:
        clc
        lda     KBD
        bpl     :+
        stx     KBDSTRB
        and     #$7F
        sec
:       rts
.endproc


.proc handle_keyboard_mouse
        lda     kbd_mouse_state
        bne     :+
        rts

:       cmp     #kbd_mouse_state_mousekeys
        beq     kbd_mouse_mousekeys

        jsr     kbd_mouse_sync_cursor

        lda     kbd_mouse_state
        cmp     #kbd_mouse_state_menu
        bne     :+
        jmp     kbd_mouse_do_menu

:       jmp     kbd_mouse_do_window
.endproc


.proc stash_cursor
        jsr     stash_addr
        copy16  active_cursor, kbd_mouse_cursor_stash
        copy16  pointer_cursor_addr, params_addr
        jsr     SetCursorImpl
        jmp     restore_addr
.endproc

kbd_mouse_cursor_stash:
        .res    2

stash_addr:
        copy16  params_addr, stashed_addr
        rts

restore_addr:
        copy16  stashed_addr, params_addr
        rts

stashed_addr:  .addr     0


.proc kbd_mouse_mousekeys
        jsr     compute_modifiers ; C=_ A=____ __SO
        ror     a                 ; C=O A=____ ___S
        ror     a                 ; C=S A=O___ ____
        ror     kbd_mouse_status  ; shift solid apple into bit 7 of kbd_mouse_status
        lda     kbd_mouse_status  ; becomes mouse button
        sta     mouse_status
        lda     #0
        sta     input__modifiers

        jsr     get_key::no_modifiers
        bcc     :+
        jmp     mousekeys_input

:       jmp     position_kbd_mouse
.endproc


.proc activate_keyboard_mouse
        pha                     ; save modifiers
        lda     kbd_mouse_state
        bne     in_kbd_mouse    ; branch away if keyboard mouse is active
        pla
        cmp     #3              ; open apple+solid apple
        bne     ret
        bit     mouse_status
        bmi     ret             ; branch away if button is down

        lda     #4
        sta     kbd_mouse_state

        ldx     #10
beeploop:
        lda     SPKR            ; Beep
        ldy     #0
:       dey
        bne     :-
        dex
        bpl     beeploop

waitloop:
        jsr     compute_modifiers
        cmp     #3
        beq     waitloop        ; wait for user to release OA+SA
        sta     input__modifiers

        lda     #0
        sta     kbd_mouse_status ; reset mouse button status
        ldx     #2
:       lda     set_pos_params,x
        sta     kbd_mouse_x,x
        dex
        bpl     :-
ret:    rts

in_kbd_mouse:
        cmp     #kbd_mouse_state_mousekeys
        bne     pla_ret
        pla
        and     #1              ; modifiers
        bne     :+
        lda     #0
        sta     kbd_mouse_state
:       rts

pla_ret:
        pla
        rts
.endproc


.proc kbd_mouse_sync_cursor
        bit     mouse_status
        bpl     :+

        lda     #0
        sta     kbd_mouse_state
        jmp     set_mouse_pos_from_kbd_mouse

:       lda     mouse_status
        pha
        lda     #$C0
        sta     mouse_status
        pla
        and     #$20
        beq     kbd_mouse_to_mouse_jmp

        ldx     #2
:       lda     mouse_x,x
        sta     kbd_mouse_x,x
        dex
        bpl     :-

        stx     kbd_menu_select_flag           ; =$ff
        rts

kbd_mouse_to_mouse_jmp:
        jmp     kbd_mouse_to_mouse
.endproc


.proc kbd_menu_select
        php
        sei
        jsr     save_mouse_pos

        lda     #kbd_mouse_state_menu
        sta     kbd_mouse_state

        jsr     position_menu_item

        lda     #$80
        sta     mouse_status
        jsr     stash_cursor
        ldx     sel_menu_index
        jsr     get_menu

        lda     curmenu__menu_id
        sta     cur_open_menu
        jsr     draw_menu

        lda     sel_menu_item_index
        sta     cur_hilited_menu_item
        jsr     hilite_menu_item
        plp
        rts

position_menu_item:
        ldx     sel_menu_index
        jsr     get_menu

        add16lc curmenu__x_min, #5, kbd_mouse_x

        ldy     sel_menu_item_index
        lda     menu_item_y_table,y
        sta     kbd_mouse_y
        lda     #$C0
        sta     mouse_status
        jmp     position_kbd_mouse
.endproc


.proc kbd_menu_select_item
        bit     kbd_menu_select_flag
        bpl     :+

        lda     cur_hilited_menu_item
        sta     sel_menu_item_index
        ldx     cur_open_menu
        dex
        stx     sel_menu_index

        lda     #0
        sta     kbd_menu_select_flag
:       rts
.endproc


.proc kbd_mouse_do_menu
        jsr     kbd_mouse_save_zp
        jsr     :+
        jmp     kbd_mouse_restore_zp

:       jsr     get_key
        bcs     handle_menu_key
        rts
.endproc

        ;; Keyboard navigation of menu
.proc handle_menu_key
        pha
        jsr     kbd_menu_select_item
        pla
        cmp     #CHAR_ESCAPE
        bne     try_return

        lda     #0
        sta     kbd_menu_item
        sta     kbd_menu
        lda     #$80
        sta     movement_cancel
        rts

try_return:
        cmp     #CHAR_RETURN
        bne     try_up
        jsr     kbd_mouse_to_mouse
        jmp     restore_cursor

try_up:
        cmp     #CHAR_UP
        bne     try_down

uploop: dec     sel_menu_item_index
        bpl     :+

        ldx     sel_menu_index
        jsr     get_menu
        ldx     menu_item_count
        stx     sel_menu_item_index

:       ldx     sel_menu_item_index
        beq     :+
        dex
        jsr     get_menu_item

        lda     curmenuitem__options
        and     #MGTK::menuopt_disable_flag | MGTK::menuopt_item_is_filler
        bne     uploop

:       jmp     kbd_menu_select::position_menu_item

try_down:
        cmp     #CHAR_DOWN
        bne     try_right

downloop:
        inc     sel_menu_item_index

        ldx     sel_menu_index
        jsr     get_menu
        lda     sel_menu_item_index
        cmp     menu_item_count
        bcc     :+
        beq     :+

        lda     #0
        sta     sel_menu_item_index
:       ldx     sel_menu_item_index
        beq     :+
        dex
        jsr     get_menu_item
        lda     curmenuitem__options
        and     #MGTK::menuopt_disable_flag | MGTK::menuopt_item_is_filler
        bne     downloop

:       jmp     kbd_menu_select::position_menu_item

try_right:
        cmp     #CHAR_RIGHT
        bne     try_left

        lda     #0
        sta     sel_menu_item_index
        inc     sel_menu_index

        lda     sel_menu_index
        cmp     menu_count
        bcc     :+

        lda     #0
        sta     sel_menu_index
:       jmp     kbd_menu_select::position_menu_item

try_left:
        cmp     #CHAR_LEFT
        bne     nope

        lda     #0
        sta     sel_menu_item_index
        dec     sel_menu_index
        bmi     :+
        jmp     kbd_menu_select::position_menu_item

:       ldx     menu_count
        dex
        stx     sel_menu_index
        jmp     kbd_menu_select::position_menu_item

nope:   jsr     kbd_menu_by_shortcut
        bcc     :+

        lda     #$80
        sta     movement_cancel
:       rts
.endproc

.proc kbd_menu_by_shortcut
        sta     find_shortcut
        lda     set_input_modifiers
        and     #3
        sta     find_options

        lda     cur_open_menu
        pha
        lda     cur_hilited_menu_item
        pha

        lda     #find_mode_by_shortcut
        jsr     find_menu
        beq     fail

        stx     kbd_menu_item
        lda     curmenu__disabled
        bmi     fail

        lda     curmenuitem__options
        and     #MGTK::menuopt_disable_flag | MGTK::menuopt_item_is_filler
        bne     fail

        lda     curmenu__menu_id
        sta     kbd_menu
        sec
        bcs     :+

fail:   clc
:       pla
        sta     cur_hilited_menu_item
        pla
        sta     cur_open_menu
        sta     find_menu_id
        rts
.endproc


.proc kbd_menu_return
.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        php
.else
        plp
.endif
        sei
        jsr     hide_menu
        jsr     restore_cursor

        lda     kbd_menu
        sta     MenuSelectImpl__params__menu_id
        sta     cur_open_menu

        lda     kbd_menu_item
        sta     MenuSelectImpl__params__menu_item
        sta     cur_hilited_menu_item

        jsr     restore_params_active_port
        lda     kbd_menu
        beq     :+
        jsr     HiliteMenuImpl

        lda     kbd_menu
:       sta     cur_open_menu
        ldx     kbd_menu_item
        stx     cur_hilited_menu_item
        plp
        jmp     store_xa_at_params
.endproc


.if ::Variant <> 'M'

.proc kbd_win_drag_or_grow
        php
        sei
        jsr     save_mouse_pos
        lda     #$80
        sta     mouse_status

        jsr     get_winframerect
        bit     drag_resize_flag
        bpl     do_drag

        lda     current_winfo__options
        and     #MGTK::option_grow_box
        beq     no_grow

        ldx     #0
:       sec
        lda     winrect__x2,x
        sbc     #4
        sta     kbd_mouse_x,x
        sta     drag_initialpos,x
        sta     drag_curpos,x

        lda     winrect__x2+1,x
        sbc     #0
        sta     kbd_mouse_x+1,x
        sta     drag_initialpos+1,x
        sta     drag_curpos+1,x

        inx
        inx
        cpx     #4
        bcc     :-

        sec
        lda     #<(screen_width-1)
        sbc     drag_initialpos__xcoord
        lda     #>(screen_width-1)
        sbc     drag_initialpos__xcoord+1
        bmi     no_grow

        sec
        lda     #<(screen_height-1)
        sbc     drag_initialpos__ycoord
        lda     #>(screen_height-1)
        sbc     drag_initialpos__ycoord+1
        bmi     no_grow
        jsr     position_kbd_mouse
        jsr     stash_cursor
        plp
        rts

no_grow:
        lda     #0
        sta     kbd_mouse_state
        lda     #MGTK::error_window_not_resizable
        plp
        jmp     MGTK__exit_with_a

do_drag:
        lda     current_winfo__options
        and     #MGTK::option_dialog_box
        beq     no_dialog

        lda     #0
        sta     kbd_mouse_state
        exit_call MGTK::error_window_not_draggable

no_dialog:
        ldx     #0
dragloop:
        clc
        lda     winrect__x1,x
        cpx     #2
        beq     is_y
.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        adc     #35
.else
        adc     #20
.endif
        jmp     :+

is_y:   adc     #5
:       sta     kbd_mouse_x,x
        sta     drag_initialpos,x
        sta     drag_curpos,x

        lda     winrect__x1+1,x
        adc     #0
        sta     kbd_mouse_x+1,x
        sta     drag_initialpos+1,x
        sta     drag_curpos+1,x
        inx
        inx
        cpx     #4
        bcc     dragloop

        bit     kbd_mouse_x+1
        bpl     xpositive

        ldx     #1
        lda     #0
:       sta     kbd_mouse_x,x
        sta     drag_initialpos,x
        sta     drag_curpos,x
        dex
        bpl     :-

xpositive:
        jsr     position_kbd_mouse
        jsr     stash_cursor
        plp
        rts
.endproc

.else
        kbd_win_drag_or_grow := 0
        .scope kbd_win_drag_or_grow
        is_y := 0
        xpositive := 0
        no_grow := 0
        dragloop := 0
        do_drag := 0
        no_dialog := 0
        .endscope
.endif


.proc kbd_mouse_add_to_y
        php
        clc
        adc     kbd_mouse_y
        sta     kbd_mouse_y
        plp
        bpl     yclamp
        cmp     #<screen_height
        bcc     :+
        lda     #0
        sta     kbd_mouse_y
:       jmp     position_kbd_mouse

yclamp: cmp     #<screen_height
        bcc     :-
        lda     #<(screen_height-1)
        sta     kbd_mouse_y
        bne     :-                  ; always
.endproc


.proc kbd_mouse_do_window
        jsr     kbd_mouse_save_zp
        jsr     :+
        jmp     kbd_mouse_restore_zp

:       jsr     get_key
        bcs     :+
        rts

:       cmp     #CHAR_ESCAPE
        bne     :+

        lda     #$80
        sta     movement_cancel
        jmp     restore_cursor

:       cmp     #CHAR_RETURN
        bne     :+
        jmp     restore_cursor

:       pha
        lda     set_input_modifiers
        beq     :+
        ora     #$80
        sta     set_input_modifiers
:       pla
        ldx     #$C0
        stx     mouse_status
        ;; Fall-through
.endproc

.proc mousekeys_input
        cmp     #CHAR_UP
        bne     not_up

        lda     #256-8
        bit     set_input_modifiers
        bpl     :+
        lda     #256-48
:       jmp     kbd_mouse_add_to_y

not_up:
        cmp     #CHAR_DOWN
        bne     not_down

        lda     #8
        bit     set_input_modifiers
        bpl     :+
        lda     #48
:       jmp     kbd_mouse_add_to_y

not_down:
        cmp     #CHAR_RIGHT
        bne     not_right

        jsr     kbd_mouse_check_xmax
        bcc     out_of_bounds

        clc
        lda     #8
        bit     set_input_modifiers
        bpl     :+
        lda     #64

:       adc     kbd_mouse_x
        sta     kbd_mouse_x
        lda     kbd_mouse_x+1
        adc     #0
        sta     kbd_mouse_x+1
        sec
        lda     kbd_mouse_x
        sbc     #<(screen_width-1)
        lda     kbd_mouse_x+1
        sbc     #>(screen_width-1)
        bmi     out_of_bounds

        lda     #>(screen_width-1)
        sta     kbd_mouse_x+1
        lda     #<(screen_width-1)
        sta     kbd_mouse_x
out_of_bounds:
        jmp     position_kbd_mouse

not_right:
        cmp     #CHAR_LEFT
        bne     not_left

        jsr     kbd_mouse_check_xmin
        bcc     out_of_boundsl

        lda     kbd_mouse_x
        bit     set_input_modifiers
        bpl     :+
        sbc     #64
        jmp     move_left

:       sbc     #8
move_left:
        sta     kbd_mouse_x
        lda     kbd_mouse_x+1
        sbc     #0
        sta     kbd_mouse_x+1
        bpl     out_of_boundsl

        lda     #0
        sta     kbd_mouse_x
        sta     kbd_mouse_x+1
out_of_boundsl:
        jmp     position_kbd_mouse

not_left:
        sta     set_input_key

        ldx     #MGTK::grafport_size-1
:       lda     $A7,x
        sta     $0600,x
        dex
        bpl     :-

        lda     set_input_key
        jsr     kbd_menu_by_shortcut
        php

        ldx     #MGTK::grafport_size-1
:       lda     $0600,x
        sta     $A7,x
        dex
        bpl     :-

        plp
        bcc     :+

        lda     #$40
        sta     movement_cancel
        jmp     restore_cursor

:       rts
.endproc


.proc set_input
.if ::Variant <> 'P'
        MGTK_CALL MGTK::PostEvent, set_input_params
.endif
        rts
.endproc

.proc set_input_params          ; 1 byte shorter than normal, since KEY
state:  .byte   MGTK::event_kind_key_down
key:    .byte   0
modifiers:
        .byte   0
.endproc
        set_input_key := set_input_params::key
        set_input_modifiers := set_input_params::modifiers

        ;; Set to true to force the return value of check_if_changed to true
        ;; during a tracking operation.
force_tracking_change:
        .byte   0


.if ::Variant <> 'M'

.proc kbd_mouse_check_xmin
        lda     kbd_mouse_state
        cmp     #kbd_mouse_state_mousekeys
        beq     ret_ok

        lda     kbd_mouse_x
        bne     ret_ok
        lda     kbd_mouse_x+1
        bne     ret_ok

        bit     drag_resize_flag
        bpl     :+
ret_ok: sec
        rts

:       jsr     get_winframerect
        lda     winrect__x2+1
        bne     min_ok
        lda     #9
        bit     set_input_params::modifiers
        bpl     :+

        lda     #65
:       cmp     winrect__x2
        bcc     min_ok
        clc
        rts

min_ok: inc     force_tracking_change

        clc
        lda     #8
        bit     set_input_params::modifiers
        bpl     :+
        lda     #64

:       adc     drag_initialpos
        sta     drag_initialpos
        bcc     :+
        inc     drag_initialpos+1
:       clc
        rts
.endproc


.proc kbd_mouse_check_xmax
        lda     kbd_mouse_state
        cmp     #kbd_mouse_state_mousekeys
        beq     :+

        bit     drag_resize_flag
        bmi     :+

        lda     kbd_mouse_x
        sbc     #<(screen_width-1)
        lda     kbd_mouse_x+1
        sbc     #>(screen_width-1)
        beq     is_max
        sec

:       sec
        rts

is_max: jsr     get_winframerect
        sec
        lda     #<(screen_width-1)
        sbc     winrect__x1
        tax
        lda     #>(screen_width-1)
        sbc     winrect__x1+1
        beq     :+

        ldx     #256-1
:       bit     set_input_modifiers
        bpl     :+

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        cpx     #100
.else
        cpx     #85
.endif
        bcc     clc_rts
        bcs     ge_100

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
:       cpx     #44
.else
:       cpx     #29
.endif
        bcc     clc_rts
        bcs     in_range

clc_rts:
        clc
        rts

ge_100: sec
        lda     drag_initialpos
        sbc     #64
        jmp     :+

in_range:
        sec
        lda     drag_initialpos
        sbc     #8
:       sta     drag_initialpos
        bcs     :+
        dec     drag_initialpos+1
:
        inc     force_tracking_change
        clc
        rts
.endproc


grew_flag:
        .byte   0

.proc set_grew_flag
        lda     #$80
        sta     grew_flag
grts:   rts
.endproc


.proc finish_grow
        bit     kbd_mouse_state
        bpl     set_grew_flag::grts

        bit     grew_flag
        bpl     set_grew_flag::grts

        jsr     get_winframerect
        php
        sei
        ldx     #0
:       sub16lc winrect__x2,x, #4, kbd_mouse_x,x
        inx
        inx
        cpx     #4
        bcc     :-

        jsr     position_kbd_mouse
        plp
        rts
.endproc


.else


;;; Keyboard mouse stubs used when windows support is not compiled in.

.proc kbd_mouse_check_xmin
        ret_ok := 0
        min_ok := 0

        sec
        rts
.endproc

        clc         ; unreferenced ???
        rts

.proc kbd_mouse_check_xmax
        in_range := 0
        clc_rts := 0
        ge_100 := 0
        is_max := 0

        ;; Fall through
.endproc

        grew_flag := 0

.proc set_grew_flag
        sec
grts:   rts
.endproc

.proc finish_grow
        clc
        rts
.endproc

.endif


;;; ============================================================
;;; ScaleMouse

;;; Sets up mouse clamping

;;; 2 bytes of params, copied to $82
;;; byte 1 controls x clamp, 2 controls y clamp
;;; clamp is to fractions of screen (0 = full, 1 = 1/2, 2 = 1/4, 3 = 1/8) (why???)

.proc ScaleMouseImpl
        params := $82
        lda     params+0
        sta     mouse_scale_x
        lda     params+1
        sta     mouse_scale_y

set_clamps:
        bit     no_mouse_flag   ; called after INITMOUSE
        bmi     end

        lda     mouse_scale_x
        asl     a
        tay
        lda     #0
        sta     mouse_x
        sta     mouse_x+1
        bit     mouse_hooked_flag
        bmi     :+

        sta     CLAMP_MIN_LO
        sta     CLAMP_MIN_HI

:       lda     clamp_x_table,y
        sta     mouse_y
        bit     mouse_hooked_flag
        bmi     :+

        sta     CLAMP_MAX_LO

:       lda     clamp_x_table+1,y
        sta     mouse_y+1
        bit     mouse_hooked_flag
        bmi     :+
        sta     CLAMP_MAX_HI
:       lda     #CLAMP_X
        ldy     #CLAMPMOUSE
        jsr     call_mouse

        lda     mouse_scale_y
        asl     a
        tay
        lda     #0
        sta     mouse_x
        sta     mouse_x+1
        bit     mouse_hooked_flag
        bmi     :+
        sta     CLAMP_MIN_LO
        sta     CLAMP_MIN_HI
:       lda     clamp_y_table,y
        sta     mouse_y
        bit     mouse_hooked_flag
        bmi     :+
        sta     CLAMP_MAX_LO
:       lda     clamp_y_table+1,y
        sta     mouse_y+1
        bit     mouse_hooked_flag
        bmi     :+
        sta     CLAMP_MAX_HI
:       lda     #CLAMP_Y
        ldy     #CLAMPMOUSE
        jsr     call_mouse
end:    rts

clamp_x_table:  .word   screen_width-1, screen_width/2-1, screen_width/4-1, screen_width/8-1
clamp_y_table:  .word   screen_height-1, screen_height/2-1, screen_height/4-1, screen_height/8-1

.endproc

;;; ============================================================
;;; Locate Mouse Slot


        ;; If X's high bit is set, only slot in low bits is tested.
        ;; Otherwise all slots are scanned.

.proc find_mouse
        txa
        and     #$7F
        beq     scan
        jsr     check_mouse_in_a
        sta     no_mouse_flag
        beq     found
        ldx     #0
        rts

        ;; Scan for mouse starting at slot 7
scan:   ldx     #7
loop:   txa
        jsr     check_mouse_in_a
        sta     no_mouse_flag
        beq     found
        dex
        bpl     loop
        ldx     #0              ; no mouse found
        rts

found:  ldy     #INITMOUSE
        jsr     call_mouse
        jsr     ScaleMouseImpl::set_clamps
        ldy     #HOMEMOUSE
        jsr     call_mouse
        lda     mouse_firmware_hi
        and     #$0F
        tax                     ; return with mouse slot in X
        rts

        ;; Check for mouse in slot A
.proc check_mouse_in_a
        ptr := $88

        ora     #>$C000
        sta     ptr+1
        lda     #<$0000
        sta     ptr

        ldy     #$0C            ; $Cn0C = $20
        lda     (ptr),y
        cmp     #$20
        bne     nope

        ldy     #$FB            ; $CnFB = $D6
        lda     (ptr),y
        cmp     #$D6
        bne     nope

        lda     ptr+1           ; yay, found it!
        sta     mouse_firmware_hi
        asl     a
        asl     a
        asl     a
        asl     a
        sta     mouse_operand
        return  #$00

nope:   return  #$80
.endproc
.endproc

no_mouse_flag:               ; high bit set if no mouse present
        .byte   0
mouse_firmware_hi:           ; e.g. if mouse is in slot 4, this is $C4
        .byte   0
mouse_operand:               ; e.g. if mouse is in slot 4, this is $40
        .byte   0

        .include "mouse-exp.inc"

