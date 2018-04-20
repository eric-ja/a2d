;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        .import sel_menu_index
        .import sel_menu_item_index


.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)

;;; ============================================================

;;; $4E SetMenuSelection

;;; 2 bytes of params, copied to $82

.proc SetMenuSelectionImpl
        params := $82

        lda     params+0
        sta     sel_menu_index

        lda     params+1
        sta     sel_menu_item_index

        rts
.endproc

.else
        SetMenuSelectionImpl := 0
        .scope SetMenuSelectionImpl
        params := 0
        .endscope
.endif


        .import BitBltImpl
        .importzp BitBltImpl__params

        ;; Extra API

        MGTK_DECL_API  BitBlt,           $4D, BitBltImpl, 0, BitBltImpl__params, 16

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        MGTK_DECL_API  SetMenuSelection, $4E, SetMenuSelectionImpl, 0, ::params, 2
.endif

        .include "setmenuselection-exp.inc"
