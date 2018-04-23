;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "apple2.inc"
        .include "../inc/apple2.inc"
        .include "../inc/prodos.inc"
        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        MGTK_IMPORT FlushEventsImpl
        MGTK_IMPORT HideCursorImpl
        MGTK_IMPORT ShowCursorImpl
        MGTK_IMPORT InitGrafImpl

        MGTK_IMPORT call_mouse
.if ::Variant <> 'M'
        MGTK_IMPORT current_window
.endif
        MGTK_IMPORT interrupt_handler
        MGTK_IMPORT mouse_hook
        MGTK_IMPORT mouse_state
        MGTK_IMPORT prepare_port
        MGTK_IMPORT preserve_zp_flag
        MGTK_IMPORTZP screen_height
        MGTK_IMPORT screen_width
        MGTK_IMPORT stack_ptr_stash
        MGTK_IMPORT standard_port
        MGTK_IMPORT standard_port__textback
        MGTK_IMPORT standard_port__textfont
        MGTK_IMPORT store_xa_at_y
        MGTK_IMPORT set_pointer_cursor
        MGTK_IMPORT find_mouse
        MGTK_IMPORT mouse_scale_x
        MGTK_IMPORT mouse_scale_y
        MGTK_IMPORT menu_item_y_table
        MGTK_IMPORT menu_item_y_table_end
.if ::Variant <> 'M'
        MGTK_IMPORT winframe_top
        MGTK_IMPORT set_port_top
.endif
        MGTK_IMPORT fill_rect_params4_top
        MGTK_IMPORT test_rect_params2_top
        MGTK_IMPORT test_rect_bottom
.if ::Variant <> 'M'
        MGTK_IMPORT wintitle_height
.endif
        MGTK_IMPORT fill_rect_params2_height
.if ::Variant <> 'M'
        MGTK_IMPORT goaway_height
.endif
        MGTK_IMPORT sysfont_height
        MGTK_IMPORT savebehind_buffer

        .import MGTK__SetPortBits
        .import MGTK__PaintRect
        .import MGTK__SetPattern


;;; Init parameters

machid: .byte   0
subid:  .byte   0
op_sys: .byte   $00
slot_num:
        .byte   $00
use_interrupts:
        .byte   $00

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
always_handle_irq:
        .byte   $00
.else
always_handle_irq := $ffff
.endif

savebehind_size:
        .res    2
savebehind_usage:
        .res    2

desktop_initialized_flag:
        .byte   0

save_p_reg:
        .byte   $00


;;; ============================================================
;;; StartDeskTop

;;; 12 bytes of params, copied to $82

.proc StartDeskTopImpl
        PARAM_BLOCK params, $82
machine:    .res 1
subid:      .res 1
op_sys:     .res 1
slot_num:   .res 1
use_irq:    .res 1
sysfontptr: .res 2
savearea:   .res 2
savesize:   .res 2
            .res 1      ; unknown
        END_PARAM_BLOCK


        php
        pla
        sta     save_p_reg

        ldx     #4
:       lda     params::machine,x
        sta     machid,x
        dex
        bpl     :-

        lda     #$7F
        sta     standard_port__textback

        copy16  params::sysfontptr, standard_port__textfont
        copy16  params::savearea, savebehind_buffer
        copy16  params::savesize, savebehind_size

        jsr     set_irq_mode
        jsr     set_op_sys

        ldy     #MGTK::font_offset_height
        lda     (params::sysfontptr),y
        tax
        stx     sysfont_height
        dex
.if ::Variant <> 'M'
        stx     goaway_height                 ; goaway height = font height - 1
.endif
        inx
        inx
        inx
        stx     fill_rect_params2_height      ; menu bar height = font height + 2

        inx
.if ::Variant <> 'M'
        stx     wintitle_height               ; win title height = font height + 3
.endif

        stx     test_rect_bottom
        stx     test_rect_params2_top
        stx     fill_rect_params4_top

        inx                                   ; font height + 4: top of desktop area
.if ::Variant <> 'M'
        stx     set_port_top
        stx     winframe_top
.endif
        stx     desktop_port_y
        stx     fill_rect_top

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        dex
        stx     menu_item_y_table

        clc
        ldy     #$00
:       txa
        adc     menu_item_y_table,y
        iny
        sta     menu_item_y_table,y
        cpy     #menu_item_y_table_end - menu_item_y_table-1
        bcc     :-
.endif

        lda     #1
        sta     mouse_scale_x
        lda     #0
        sta     mouse_scale_y

        bit     subid
        bvs     :+

        lda     #2                       ; default scaling for IIc/IIc+
        sta     mouse_scale_x
        lda     #1
        sta     mouse_scale_y
:
        ldx     slot_num
        jsr     find_mouse

        bit     slot_num
        bpl     found_mouse
        cpx     #0
        bne     :+
        exit_call MGTK::error_no_mouse

:       lda     slot_num
        and     #$7F
        beq     found_mouse
        cpx     slot_num
        beq     found_mouse
        exit_call $91

found_mouse:
        stx     slot_num

        lda     #$80
        sta     desktop_initialized_flag

        lda     slot_num
        bne     no_mouse
        bit     use_interrupts
        bpl     no_mouse
        lda     #0
        sta     use_interrupts
no_mouse:

        ldy     #params::slot_num - params
        lda     slot_num
        sta     (params_addr),y
        iny
        lda     use_interrupts
        sta     (params_addr),y
        bit     use_interrupts
        bpl     no_irq
        bit     op_sys
        bpl     no_irq

        MLI_CALL ALLOC_INTERRUPT, alloc_interrupt_params

no_irq: lda     VERSION
        pha

        lda     #F8VERSION           ; F8 ROM IIe ID byte
        sta     VERSION

        ldy     #SETMOUSE
        lda     #1

        bit     use_interrupts
        bpl     :+
        cli
        ora     #8
:       jsr     call_mouse

        pla
        sta     VERSION

        jsr     InitGrafImpl
        jsr     set_pointer_cursor
        jsr     FlushEventsImpl

.if ::Variant <> 'M'
        lda     #0
        sta     current_window+1
.endif

reset_desktop:
        jsr     save_params_and_stack
        jsr     set_desktop_port

        ;; Fills the desktop background on startup (menu left black)
        MGTK_CALL MGTK__SetPattern, checkerboard_pattern
        MGTK_CALL MGTK__PaintRect, fill_rect_params
        jmp     restore_params_active_port
.endproc

        DEFINE_ALLOC_INTERRUPT_PARAMS alloc_interrupt_params, interrupt_handler
        DEFINE_DEALLOC_INTERRUPT_PARAMS dealloc_interrupt_params


.proc set_irq_mode
.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        lda     #0
        sta     always_handle_irq
.endif

        lda     use_interrupts
        beq     irts

        cmp     #1
.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        beq     irq_on
        cmp     #3
        bne     irq_err

        lda     #$80
        sta     always_handle_irq
.else
        bne     irq_err
.endif
irq_on:
        lda     #$80
        sta     use_interrupts
irts:   rts

irq_err:
        exit_call MGTK::error_invalid_irq_setting
.endproc

.proc set_op_sys
        lda     op_sys
        beq     is_prodos
        cmp     #1
        beq     is_pascal

        exit_call MGTK::error_invalid_op_sys

is_prodos:
        lda     #$80
        sta     op_sys
is_pascal:
        rts
.endproc

;;; ============================================================
;;; StopDeskTop

.proc StopDeskTopImpl
        ldy     #SETMOUSE
        lda     #MOUSE_MODE_OFF
        jsr     call_mouse
        ldy     #SERVEMOUSE
        jsr     call_mouse
        bit     use_interrupts

        bpl     :+
        bit     op_sys
        bpl     :+
        lda     alloc_interrupt_params::int_num
        sta     dealloc_interrupt_params::int_num
        MLI_CALL DEALLOC_INTERRUPT, dealloc_interrupt_params
:
        lda     save_p_reg
        pha
        plp
        lda     #0
        sta     desktop_initialized_flag
        rts
.endproc

;;; ============================================================
;;; SetUserHook

;;; 3 bytes of params, copied to $82

.proc SetUserHookImpl
        params := $82

        lda     $82
        cmp     #1
        bne     :+

        lda     $84
        bne     clear_before_events_hook
        sta     before_events_hook+1
        lda     $83
        sta     before_events_hook
        rts

:       cmp     #2
        bne     invalid_hook

        lda     $84
        bne     clear_after_events_hook
        sta     after_events_hook+1
        lda     $83
        sta     after_events_hook
        rts

clear_before_events_hook:
        lda     #0
        sta     before_events_hook
        sta     before_events_hook+1
        rts

clear_after_events_hook:
        lda     #0
        sta     after_events_hook
        sta     after_events_hook+1
        rts

invalid_hook:
        exit_call MGTK::error_invalid_hook
.endproc


.proc call_before_events_hook
        lda     before_events_hook+1
        beq     :+
        jsr     save_params_and_stack

        jsr     before_events_hook_jmp
        php
        jsr     restore_params_active_port
        plp
:       rts

before_events_hook_jmp:
        jmp     (before_events_hook)
.endproc


before_events_hook:
        .res    2


.proc call_after_events_hook
        lda     after_events_hook+1
        beq     :+
        jsr     save_params_and_stack

        jsr     after_events_hook_jmp
        php
        jsr     restore_params_active_port
        plp
:       rts

after_events_hook_jmp:
        jmp     (after_events_hook)
.endproc


after_events_hook:
        .res    2


params_addr_save:
        .res    2

stack_ptr_save:
        .res    1


.proc hide_cursor_save_params
        jsr     HideCursorImpl
        ;; Fall-through
.endproc

.proc save_params_and_stack
        copy16  params_addr, params_addr_save
        lda     stack_ptr_stash
        sta     stack_ptr_save
        lsr     preserve_zp_flag
        rts
.endproc


.proc show_cursor_and_restore
        jsr     ShowCursorImpl
        ;; Fall-through
.endproc

.proc restore_params_active_port
        asl     preserve_zp_flag
        copy16  params_addr_save, params_addr
        ldax    active_port
        ;; Fall-through
.endproc

.proc set_and_prepare_port
        stax    $82
        lda     stack_ptr_save
        sta     stack_ptr_stash

        ldy     #MGTK::grafport_size-1
:       lda     ($82),y
        sta     current_grafport,y
        dey
        bpl     :-
        jmp     prepare_port
.endproc


.proc set_standard_port
        ldax    standard_port_addr
        bne     set_and_prepare_port                  ; always
.endproc

standard_port_addr:
        .addr   standard_port


.proc set_desktop_port
        jsr     set_standard_port
        MGTK_CALL MGTK__SetPortBits, desktop_port_bits
        rts

desktop_port_bits:
        .word   0               ; viewloc x
port_y:
        .word   13              ; viewloc y = font height + 4
        .word   $2000           ; mapbits
        .byte   $80             ; mapwidth
        .res    1               ; reserved
.endproc

desktop_port_y := set_desktop_port::port_y


.proc fill_rect_params
left:   .word   0
top:    .word   0
right:  .word   screen_width-1
bottom: .word   screen_height-1
.endproc
        fill_rect_top := fill_rect_params::top

        .byte   $00,$00,$00,$00,$00,$00,$00,$00

checkerboard_pattern:
        .byte   %01010101
        .byte   %10101010
        .byte   %01010101
        .byte   %10101010
        .byte   %01010101
        .byte   %10101010
        .byte   %01010101
        .byte   %10101010
        .byte   $00

;;; ============================================================
;;; AttachDriver

;;; 2 bytes of params, copied to $82

.proc AttachDriverImpl
        params := $82

        bit     desktop_initialized_flag
        bmi     fail

        copy16  params, mouse_hook

        ldax    mouse_state_addr
        ldy     #2
        jmp     store_xa_at_y

fail:   exit_call MGTK::error_desktop_already_initialized

mouse_state_addr:
        .addr   mouse_state
.endproc


        MGTK_IMPORT ScaleMouseImpl
        MGTK_IMPORTZP ScaleMouseImpl__params
        MGTK_IMPORT KeyboardMouseImpl
        MGTK_IMPORTZP KeyboardMouseImpl__params

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        MGTK_IMPORT GetIntHandlerImpl
.endif

        MGTK_IMPORT SetCursorImpl
        MGTK_IMPORT ObscureCursorImpl
        MGTK_IMPORT GetCursorAddrImpl


.if ::Variant <> 'P'

        ;; ----------------------------------------
        ;; MouseGraphics ToolKit Calls

        ;; Initialization
        MGTK_DECL_API  StartDeskTop,   $1D, StartDeskTopImpl, 0, ::params
        MGTK_DECL_API  StopDeskTop,    $1E, StopDeskTopImpl, 0
        MGTK_DECL_API  SetUserHook,    $1F, SetUserHookImpl, 0, ::params, 3
        MGTK_DECL_API  AttachDriver,   $20, AttachDriverImpl, 0, ::params, 2
        MGTK_DECL_API  ScaleMouse,     $21, ScaleMouseImpl, 0, ScaleMouseImpl__params, 2
        MGTK_DECL_API  KeyboardMouse,  $22, KeyboardMouseImpl, 0, KeyboardMouseImpl__params, 1
.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        MGTK_DECL_API  GetIntHandler,  $23, GetIntHandlerImpl, 0
.endif

        ;; Cursor Manager
        MGTK_DECL_API  SetCursor,      $24, SetCursorImpl, 0
        MGTK_DECL_API  ShowCursor,     $25, ShowCursorImpl, 0
        MGTK_DECL_API  HideCursor,     $26, HideCursorImpl, 0
        MGTK_DECL_API  ObscureCursor,  $27, ObscureCursorImpl, 0
        MGTK_DECL_API  GetCursorAddr,  $28, GetCursorAddrImpl, 0

.endif


        MGTK_DECL_ERROR  error_invalid_op_sys                ; $90
        MGTK_DECL_ERROR  error_unknown91                     ; $91
        MGTK_DECL_ERROR  error_no_mouse                      ; $92
        MGTK_DECL_ERROR  error_invalid_irq_setting           ; $93
        MGTK_DECL_ERROR  error_invalid_hook                  ; $94
        MGTK_DECL_ERROR  error_desktop_already_initialized   ; $95
        MGTK_DECL_ERROR  error_unknown96                     ; $96
        MGTK_DECL_ERROR  error_irq_in_use                    ; $97


        .include "start-exp.inc"
