;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        PARAM_BLOCK winrect, $C7
x1:        .word   0
y1:        .word   0
x2:        .word   0
y2:        .word   0
        END_PARAM_BLOCK

        .include "zp-exp.inc"
