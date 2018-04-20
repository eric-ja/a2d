;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


;;; ============================================================

low_zp_stash_buffer:
poly_maxima_yh_table:
        .res    16

poly_maxima_x_frach:
        .res    16

poly_maxima_x_fracl:
        .res    16

poly_maxima_xl_table:
        .res    16

poly_maxima_xh_table:
        .res    16

        .include "polybufs-exp.inc"
