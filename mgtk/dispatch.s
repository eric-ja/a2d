;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "apple2.inc"
        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


;;; ============================================================
;;; Call Dispatcher

        MGTK_IMPORT desktop_initialized_flag
        MGTK_IMPORT active_saved
        MGTK_IMPORT zp_saved
        MGTK_IMPORT stack_ptr_stash
        MGTK_IMPORT preserve_zp_flag
        MGTK_IMPORT HideCursorImpl
        MGTK_IMPORT ShowCursorImpl


        .segment "MGTK_DISPATCH"


.proc dispatch

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        lda     LOWSCR
        sta     SET80COL
.endif

        bit     preserve_zp_flag ; save ZP?
        bpl     adjust_stack

        ;; Save $80...$FF, swap in what MGTK needs at $F4...$FF
        ldx     #$7F
:       lda     $80,x
        sta     zp_saved,x
        dex
        bpl     :-
        ldx     #$0B
:       lda     active_saved,x
        sta     active_port,x
        dex
        bpl     :-
        jsr     apply_active_port_to_port

adjust_stack:                   ; Adjust stack to account for params
        pla                     ; and stash address at params_addr.
        sta     params_addr
        clc
        adc     #<3
        tax
        pla
        sta     params_addr+1
        adc     #>3
        pha
        txa
        pha

        tsx
        stx     stack_ptr_stash

        ldy     #1              ; Command index
        lda     (params_addr),y
        asl     a
        tax
        copy16  jump_table,x, jump_addr

        iny                     ; Point params_addr at params
        lda     (params_addr),y
        pha
        iny
        lda     (params_addr),y
        sta     params_addr+1
        pla
        sta     params_addr

        ;; Param length format is a byte pair;
        ;; * first byte is ZP address to copy bytes to
        ;; * second byte's high bit is "hide cursor" flag
        ;; * rest of second byte is # bytes to copy

        ldy     param_lengths+1,x ; Check param length...
        bpl     done_hiding

        txa                     ; if high bit was set, stash
        pha                     ; registers and params_addr and then
        tya                     ; optionally hide cursor
        pha
        lda     params_addr
        pha
        lda     params_addr+1
        pha
        bit     desktop_initialized_flag
        bpl     :+
        jsr     hide_cursor
:       pla
        sta     params_addr+1
        pla
        sta     params_addr
        pla
        and     #$7F            ; clear high bit in length count
        tay
        pla
        tax

done_hiding:
        lda     param_lengths,x ; ZP offset for params
        beq     jump            ; nothing to copy
        sta     store+1
        dey
:       lda     (params_addr),y
store:  sta     $FF,y           ; self modified
        dey
        bpl     :-

        jump_addr := *+1
jump:   jsr     $FFFF           ; the actual call

        ;; Exposed for routines to call directly
cleanup:
        bit     desktop_initialized_flag
        bpl     :+
        jsr     show_cursor

:       bit     preserve_zp_flag
        bpl     exit_with_0
        jsr     apply_port_to_active_port
        ldx     #$0B
:       lda     active_port,x
        sta     active_saved,x
        dex
        bpl     :-
        ldx     #$7F
:       lda     zp_saved,x
        sta     $80,x
        dex
        bpl     :-

        ;; default is to return with A=0
exit_with_0:
        lda     #0

rts1:   rts
.endproc

;;; ============================================================
;;; Routines can jmp here to exit with A set

exit_with_a:
        pha
        jsr     dispatch::cleanup
        pla
        ldx     stack_ptr_stash
        txs
        ldy     #$FF
rts2:   rts

;;; ============================================================
;;; Copy port params (36 bytes) to/from active port addr

.proc apply_active_port_to_port
        ldy     #MGTK::grafport_size-1
:       lda     (active_port),y
        sta     current_grafport,y
        dey
        bpl     :-
        rts
.endproc

.proc apply_port_to_active_port
        ldy     #MGTK::grafport_size-1
:       lda     current_grafport,y
        sta     (active_port),y
        dey
        bpl     :-
        rts
.endproc

;;; ============================================================
;;; Drawing calls show/hide cursor before/after
;;; A recursion count is kept to allow rentrancy.

hide_cursor_count:
        .byte   0

.proc hide_cursor
        dec     hide_cursor_count
        jmp     HideCursorImpl
.endproc

.proc show_cursor
        bit     hide_cursor_count
        bpl     rts2
        inc     hide_cursor_count
        jmp     ShowCursorImpl
.endproc

;;; ============================================================
;;; Jump table for MGTK entry point calls

        ;; jt_rts can be used if the only thing the
        ;; routine needs to do is copy params into
        ;; the zero page (port)
        jt_rts := dispatch::rts1


        .import __MGTK_JUMPTABLE_RUN__
jump_table := __MGTK_JUMPTABLE_RUN__

        .import __MGTK_PARAMTABLE_RUN__
param_lengths := __MGTK_PARAMTABLE_RUN__


        MGTK_DECL_API  NoOp, $00, jt_rts, 0


        .include "dispatch-exp.inc"
