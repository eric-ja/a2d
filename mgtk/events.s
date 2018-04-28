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


        MGTK_IMPORT activate_keyboard_mouse
        MGTK_IMPORT always_handle_irq
        MGTK_IMPORT call_after_events_hook
        MGTK_IMPORT call_before_events_hook
        MGTK_IMPORT call_mouse
        MGTK_IMPORT kbd_mouse_state
        MGTK_IMPORT mouse_status
        MGTK_IMPORT move_cursor
        MGTK_IMPORT set_mouse_pos
        MGTK_IMPORT set_pos_params
        MGTK_IMPORT store_xa_at_params
        MGTK_IMPORT use_interrupts
        MGTK_IMPORT exit_with_a
        MGTK_IMPORTZP error_irq_in_use


;;; ============================================================
;;; PeekEvent

.proc PeekEventImpl
        clc
        bcc     GetEventImpl_peek_entry
.endproc


;;; ============================================================
;;; GetEvent

.proc GetEventImpl
        sec
peek_entry:
        php
        bit     use_interrupts
        bpl     :+
        sei
        bmi     no_check

:       jsr     CheckEventsImpl

no_check:
        jsr     next_event
        bcs     no_event

        plp
        php
        bcc     :+              ; skip advancing tail mark if in peek mode
        sta     eventbuf_tail

:       tax
        ldy     #0              ; Store 5 bytes at params
:       lda     eventbuf,x
        sta     (params_addr),y
        inx
        iny
        cpy     #4
        bne     :-
        lda     #0
        sta     (params_addr),y
        beq     ret

no_event:
        jsr     return_move_event

ret:    plp
        bit     use_interrupts
        bpl     :+
        cli
:       rts
.endproc

GetEventImpl_peek_entry := GetEventImpl::peek_entry


;;; ============================================================

;;; 5 bytes of params, copied to $82

.proc PostEventImpl
        PARAM_BLOCK params, $82
kind:   .byte    0
xcoord: .word    0           ; also used for key/modifiers/window id
ycoord: .word    0
        END_PARAM_BLOCK

        php
        sei
        lda     params::kind
        bmi     event_ok

        cmp     #MGTK::event_kind_update
        bcs     bad_event
        cmp     #MGTK::event_kind_key_down
        beq     event_ok

        ldx     params::xcoord
        ldy     params::xcoord+1
        lda     params::ycoord
        jsr     set_mouse_pos

event_ok:
        jsr     put_event
        bcs     no_room
        tax

        ldy     #0
:       lda     (params_addr),y
        sta     eventbuf,x
        inx
        iny
        cpy     #MGTK::short_event_size
        bne     :-

        plp
        rts

bad_event:
        lda     #MGTK__error_invalid_event
        bmi     error_return

no_room:
        lda     #MGTK__error_event_queue_full
error_return:
        plp
        jmp     exit_with_a
.endproc


        ;; Return a no_event (if mouse up) or drag event (if mouse down)
        ;; and report the current mouse position.
.proc return_move_event
        lda     #MGTK::event_kind_no_event

        bit     mouse_status
        bpl     :+
        lda     #MGTK::event_kind_drag

:       ldy     #0
        sta     (params_addr),y         ; Store 5 bytes at params
        iny
:       lda     set_pos_params-1,y
        sta     (params_addr),y
        iny
        cpy     #MGTK::event_size
        bne     :-
        rts
.endproc


;;; ============================================================
;;; CheckEvents


.proc input
state:  .byte   0

key        := *
kmods      := * + 1

xpos       := *
ypos       := * + 2
modifiers  := * + 3

        .res    4, 0
.endproc

.proc CheckEventsImpl
        bit     use_interrupts
        bpl     irq_entry
        exit_call error_irq_in_use

irq_entry:
        sec                     ; called from interrupt handler
        jsr     call_before_events_hook
        bcc     end

        lda     BUTN1           ; Look at buttons (apple keys), compute modifiers
        asl     a
        lda     BUTN0
        and     #$80
        rol     a
        rol     a
        sta     input::modifiers

        jsr     activate_keyboard_mouse    ; check if keyboard mouse should be started
        jsr     move_cursor
        lda     mouse_status    ; bit 7 = is down, bit 6 = was down, still down
        asl     a
        eor     mouse_status
        bmi     :+              ; minus = (is down & !was down)

        bit     mouse_status
        bmi     end             ; minus = is down
        bit     check_kbd_flag
        bpl     :+
.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        lda     kbd_mouse_state
        bne     :+
.endif

        lda     KBD
        bpl     end             ; no key
        and     #$7F
        sta     input::key
        bit     KBDSTRB         ; clear strobe

        lda     input::modifiers
        sta     input::kmods
        lda     #MGTK::event_kind_key_down
        sta     input::state
        bne     put_key_event   ; always

:       bcc     up
        lda     input::modifiers
        beq     :+
        lda     #MGTK::event_kind_apple_key
        bne     set_state

:       lda     #MGTK::event_kind_button_down
        bne     set_state

up:     lda     #MGTK::event_kind_button_up

set_state:
        sta     input::state

        ldx     #2
:       lda     set_pos_params,x
        sta     input::key,x
        dex
        bpl     :-

put_key_event:
        jsr     put_event
        tax
        ldy     #0
:       lda     input,y
        sta     eventbuf,x
        inx
        iny
        cpy     #MGTK::short_event_size
        bne     :-

end:    jmp     call_after_events_hook
.endproc



;;; ============================================================
;;; Interrupt Handler

int_stash_zp:
        .res    9, 0

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
int_stash_rdpage2:
        .byte   0
int_stash_rd80store:
        .byte   0
.else
int_stash_rdpage2 := 0
int_stash_rd80store := 0
.endif


.proc interrupt_handler
        cld                     ; required for interrupt handlers

body:                           ; returned by GetIntHandler

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        lda     RDPAGE2         ; record softswitch state
        sta     int_stash_rdpage2
        lda     RD80STORE
        sta     int_stash_rd80store
        lda     LOWSCR
        sta     SET80COL
.endif

        ldx     #8              ; preserve 9 bytes of ZP
sloop:  lda     $82,x
        sta     int_stash_zp,x
        dex
        bpl     sloop

        ldy     #SERVEMOUSE
        jsr     call_mouse
        bcs     :+
        jsr     CheckEventsImpl::irq_entry
        clc
:
.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        bit     always_handle_irq
        bpl     :+
        clc                     ; carry clear if interrupt handled
:
.endif

        ldx     #8              ; restore ZP
rloop:  lda     int_stash_zp,x
        sta     $82,x
        dex
        bpl     rloop

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        lda     LOWSCR          ;  restore soft switches
        sta     CLR80COL
        lda     int_stash_rdpage2
        bpl     :+
        lda     HISCR
:       lda     int_stash_rd80store
        bpl     :+
        sta     SET80COL
:
.endif

        rts
.endproc


;;; ============================================================
;;; GetIntHandler

.if ::VStatus < 'B' || (::VStatus = 'B' && ::VRelease < 10)
int_handler_addr:
        .addr   interrupt_handler::body
.endif


.proc GetIntHandlerImpl
        ldax    int_handler_addr
        jmp     store_xa_at_params
.endproc


.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
int_handler_addr:
        .addr   interrupt_handler::body
.endif


;;; ============================================================
;;; FlushEvents

;;; This is called during init by the DAs, just before
;;; entering the input loop.

eventbuf_tail:  .byte   0
eventbuf_head:  .byte   0

        eventbuf_size := 33             ; max # of events in queue

eventbuf:
        .scope  eventbuf
        kind      := *
        key       := *+1
        modifiers := *+2
        window_id := *+1
        .endscope

        .res    eventbuf_size*MGTK::short_event_size


.proc FlushEventsImpl
        php
        sei
        lda     #0
        sta     eventbuf_tail
        sta     eventbuf_head
        plp
        rts
.endproc
        ;; called during PostEvent and a few other places
.proc put_event
        lda     eventbuf_head
        cmp     #(eventbuf_size-1)*MGTK::short_event_size
        bne     :+                      ; if head is not at end, advance
        lda     #0                      ; otherwise reset to 0
        bcs     compare
:       clc
        adc     #MGTK::short_event_size

compare:
        cmp     eventbuf_tail           ; did head catch up with tail?
        beq     rts_with_carry_set
        sta     eventbuf_head           ; nope, maybe next time
        clc
        rts
.endproc

rts_with_carry_set:
        sec
        rts

        ;; called during GetEvent
.proc next_event
        lda     eventbuf_tail           ; equal?
        cmp     eventbuf_head
        beq     rts_with_carry_set
        cmp     #$80
        bne     :+
        lda     #0
        bcs     ret                     ; always

:       clc
        adc     #MGTK::short_event_size
ret:    clc
        rts
.endproc


;;; ============================================================
;;; SetKeyEvent

;;; 1 byte of params, copied to $82

check_kbd_flag:  .byte   $80

.proc SetKeyEventImpl
        params := $82

        asl     check_kbd_flag
        ror     params
        ror     check_kbd_flag
        rts
.endproc


        ;; Event Manager API

.if ::Variant <> 'P'

        MGTK_DECL_API  CheckEvents,  $29, CheckEventsImpl, 0
        MGTK_DECL_API  GetEvent,     $2A, GetEventImpl, 0
        MGTK_DECL_API  FlushEvents,  $2B, FlushEventsImpl, 0
        MGTK_DECL_API  PeekEvent,    $2C, PeekEventImpl, 0
        MGTK_DECL_API  PostEvent,    $2D, PostEventImpl, 0, ::params
        MGTK_DECL_API  SetKeyEvent,  $2E, SetKeyEventImpl, 0, ::params, 1

.endif


        MGTK_DECL_ERROR  error_invalid_event       ; $98
        MGTK_DECL_ERROR  error_event_queue_full    ; $99


        .include "events-exp.inc"
