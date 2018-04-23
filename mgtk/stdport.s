;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        MGTK_IMPORT screen_width
        MGTK_IMPORTZP screen_height


;;; ============================================================

;;; Standard GrafPort

.proc standard_port
viewloc:        .word   0, 0
mapbits:        .addr   MGTK::screen_mapbits
mapwidth:       .word   MGTK::screen_mapwidth
maprect:        .word   0, 0, screen_width-1, screen_height-1
penpattern:     .res    8, $FF
colormasks:     .byte   MGTK::colormask_and, MGTK::colormask_or
penloc:         .word   0, 0
penwidth:       .byte   1
penheight:      .byte   1
mode:           .byte   0
textback:       .byte   0
textfont:       .addr   0
.endproc

;;; ============================================================

.proc saved_port
viewloc:        .word   0, 0
mapbits:        .addr   MGTK::screen_mapbits
mapwidth:       .word   MGTK::screen_mapwidth
maprect:        .word   0, 0, screen_width-1, screen_height-1
penpattern:     .res    8, $FF
colormasks:     .byte   MGTK::colormask_and, MGTK::colormask_or
penloc:         .word   0, 0
penwidth:       .byte   1
penheight:      .byte   1
mode:           .byte   0
textback:       .byte   0
textfont:       .addr   0
.endproc

active_saved:           ; saved copy of $F4...$FF when ZP swapped
        .addr   saved_port
        .res    10, 0

        .include "stdport-exp.inc"
