;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        .import dispatch__cleanup
        .import low_zp_stash_buffer
        .import low_zp_stash_flag
        .import preserve_zp_flag
        .import rts3


;;; ============================================================
;;; SetZP1

;;; 1 byte of params, copied to $82

.proc SetZP1Impl
        param := $82

        lda     param
        cmp     preserve_zp_flag
        beq     rts3
        sta     preserve_zp_flag
        bcc     rts3
        jmp     dispatch__cleanup
.endproc

        MGTK_DECL_API  SetZP1,   $1A, SetZP1Impl, 0, ::param, 1


;;; ============================================================
;;; SetZP2

;;; 1 byte of params, copied to $82

;;; If high bit set stash ZP $00-$43 to buffer if not already stashed.
;;; If high bit clear unstash ZP $00-$43 from buffer if not already unstashed.

.proc SetZP2Impl
        param := $82

        lda     param
        cmp     low_zp_stash_flag
        beq     rts3
        sta     low_zp_stash_flag
        bcc     unstash

maybe_stash:
        bit     low_zp_stash_flag
        bpl     end

        ;; Copy buffer to ZP $00-$43
stash:
        ldx     #$43
:       lda     low_zp_stash_buffer,x
        sta     $00,x
        dex
        bpl     :-

end:    rts

maybe_unstash:
        bit     low_zp_stash_flag
        bpl     end

        ;; Copy ZP $00-$43 to buffer
unstash:
        ldx     #$43
:       lda     $00,x
        sta     low_zp_stash_buffer,x
        dex
        bpl     :-
        rts
.endproc
        maybe_stash_low_zp := SetZP2Impl::maybe_stash
        maybe_unstash_low_zp := SetZP2Impl::maybe_unstash

        MGTK_DECL_API  SetZP2,   $1B, SetZP2Impl, 0, ::param, 1


        .include "setzp-exp.inc"

