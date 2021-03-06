;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        MGTK_IMPORT sel_menu_index
        MGTK_IMPORT sel_menu_item_index


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


        MGTK_IMPORT BitBltImpl
        MGTK_IMPORTZP BitBltImpl__params

        ;; Extra API

.if ::Variant <> 'P'
        MGTK_DECL_API  BitBlt,           $4D, BitBltImpl, 0, BitBltImpl__params, 16
.endif

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        MGTK_DECL_API  SetMenuSelection, $4E, SetMenuSelectionImpl, 0, ::params, 2
.endif

        .include "setmenuselection-exp.inc"
