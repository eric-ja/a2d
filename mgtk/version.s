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
;;; Version

.proc VersionImpl
        ldy     #5              ; Store 6 bytes at params
loop:   lda     version,y
        sta     (params_addr),y
        dey
        bpl     loop
        rts

.proc version
VMajor:   .byte ::VMajor
VMedium:  .byte ::VMedium
VMinor:   .byte ::VMinor
VStatus:  .byte ::VStatus
VRelease: .word ::VRelease
.endproc
.endproc

        MGTK_DECL_API  Version,   $1C, VersionImpl, 0


;;; ============================================================

preserve_zp_flag:         ; if high bit set, ZP saved during MGTK calls
        .byte   $80

low_zp_stash_flag:
        .byte   $80

stack_ptr_stash:
        .byte   0

        .include "version-exp.inc"
