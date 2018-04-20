;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        .import FlushEventsImpl
        .import kbd_mouse_state


;;; ============================================================
;;; KeyboardMouse

;;; 1 byte of params, copied to $82

.proc KeyboardMouseImpl
        params := $82

        lda     #$80
        sta     kbd_mouse_state
        jmp     FlushEventsImpl
.endproc

        .include "keyboardmouse-exp.inc"

