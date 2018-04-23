;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        MGTK_IMPORT BitBltImpl
        MGTK_IMPORTZP PaintBitsImpl__dbi_left
        MGTK_IMPORT div7_table
        MGTK_IMPORTZP icon_offset_width
        MGTK_IMPORTZP src_mapwidth


;;; ============================================================

        ;; Used to draw scrollbar arrows and resize box
.proc draw_icon
        icon_ptr := $82

        stax    icon_ptr
        ldy     #3

:       lda     #0
        sta     PaintBitsImpl__dbi_left,y
        lda     (icon_ptr),y
        sta     left,y
        dey
        bpl     :-

        iny
        sty     $91             ; zero

        ldy     #icon_offset_width
        lda     (icon_ptr),y
        tax
        lda     div7_table+7,x
        sta     src_mapwidth

        txa
        ldx     left+1
        clc
        adc     left
        bcc     :+
        inx
:       stax    right

        iny
        lda     (icon_ptr),y    ; height
        ldx     top+1
        clc
        adc     top
        bcc     :+
        inx
:       stax    bottom

        iny
        lda     (icon_ptr),y
        sta     bits_addr
        iny
        lda     (icon_ptr),y
        sta     bits_addr+1
        jmp     BitBltImpl
.endproc

        .include "drawicon-exp.inc"
