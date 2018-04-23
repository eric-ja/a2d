;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        MGTK_IMPORT   shift_1_aux
        MGTK_IMPORT   shift_2_aux
        MGTK_IMPORT   shift_3_aux
        MGTK_IMPORT   shift_4_aux
        MGTK_IMPORT   shift_5_aux
        MGTK_IMPORT   shift_6_aux
        MGTK_IMPORT   shift_1_main
        MGTK_IMPORT   shift_2_main
        MGTK_IMPORT   shift_3_main
        MGTK_IMPORT   shift_4_main
        MGTK_IMPORT   shift_5_main
        MGTK_IMPORT   shift_6_main
        MGTK_IMPORT   divmod7

        MGTK_IMPORT   dhgr_shift_bits
        MGTK_IMPORT   dhgr_shift_bits__offset1_addr
        MGTK_IMPORT   dhgr_shift_bits__offset2_addr
        MGTK_IMPORT   dhgr_shift_bits__shift_aux_addr
        MGTK_IMPORT   dhgr_shift_bits__shift_main_addr

        MGTK_IMPORT   dhgr_shift_line__offset1_addr
        MGTK_IMPORT   dhgr_shift_line__offset2_addr
        MGTK_IMPORT   shift_line_jmp
        MGTK_IMPORT   dhgr_get_srcbits__shift_bits_jmp_addr
        MGTK_IMPORT   dhgr_get_srcbits__offset1_addr
        MGTK_IMPORT   dhgr_get_srcbits__offset2_addr

        MGTK_IMPORT   set_up_fill_mode
        MGTK_IMPORT   clip_rect
        MGTK_IMPORT   bitmap_buffer
        MGTK_IMPORT   set_dest
        MGTK_IMPORT   set_source
        MGTK_IMPORT   bit_blit



;;; ============================================================
;;  PaintBits

;;; 16 bytes of params, copied to $8A

src_width_bytes:
        .res    1          ; width of source data in chars

unused_width:
        .res    1          ; holds the width of data, but is not used ???

.proc PaintBitsImpl
        params     := $8A

        dbi_left   := $8A
        dbi_top    := $8C
        dbi_bitmap := $8E     ; aka bits_addr
        dbi_stride := $90     ; aka src_mapwidth
        dbi_hoff   := $92     ; aka left
        dbi_voff   := $94     ; aka top
        dbi_width  := $96     ; aka right
        dbi_height := $98     ; aka bottom

        dbi_x      := $9B
        dbi_y      := $9D

        offset     := $82


        ldx     #3         ; copy left/top to $9B/$9D
:       lda     dbi_left,x ; and hoff/voff to $8A/$8C (overwriting left/top)
        sta     dbi_x,x
        lda     dbi_hoff,x
        sta     dbi_left,x
        dex
        bpl     :-

        sub16   dbi_width, dbi_hoff, offset
        lda     dbi_x
        sta     left

        clc
        adc     offset
        sta     right
        lda     dbi_x+1
        sta     left+1
        adc     offset+1
        sta     right+1

        sub16   dbi_height, dbi_voff, offset
        lda     dbi_y
        sta     top
        clc
        adc     offset
        sta     bottom
        lda     dbi_y+1
        sta     top+1
        adc     offset+1
        sta     bottom+1
        ;; fall through to BitBlt
.endproc

;;; ============================================================
;;; $4D BitBlt

;;; 16 bytes of params, copied to $8A

        src_byte_off      := $8A        ; char offset within source line
        bit_offset        := $9B
        shift_bytes       := $81

.proc BitBltImpl
        params := $8A

        lda     #0
        sta     clipped_left
        sta     clipped_left+1
        sta     clipped_top

        lda     bits_addr+1
        sta     $80

        jsr     clip_rect
        bcs     :+
        rts

:       jsr     set_up_fill_mode
        lda     width_bytes
        asl     a
        ldx     left_masks_table+1      ; need left mask on aux?
        beq     :+
        adc     #1
:       ldx     right_masks_table       ; need right mask on main?
        beq     :+
        adc     #1
:       sta     unused_width
        sta     src_width_bytes         ; adjusted width in chars

        lda     #2
        sta     shift_bytes
        lda     #0                      ; Calculate starting Y-coordinate
        sec                             ;  = dbi_top - clipped_top
        sbc     clipped_top
        clc
        adc     PaintBitsImpl::dbi_top
        sta     PaintBitsImpl::dbi_top

        lda     #0                      ; Calculate starting X-coordinate
        sec                             ;  = dbi_left - clipped_left
        sbc     clipped_left
        tax
        lda     #0
        sbc     clipped_left+1
        tay

        txa
        clc
        adc     PaintBitsImpl::dbi_left
        tax
        tya
        adc     PaintBitsImpl::dbi_left+1

        jsr     divmod7
        sta     src_byte_off
        tya                             ; bit offset between src and dest
        rol     a
        cmp     #7
        ldx     #1
        bcc     :+
        dex
        sbc     #7

:       stx     dhgr_get_srcbits__offset1_addr
        inx
        stx     dhgr_get_srcbits__offset2_addr
        sta     bit_offset

        lda     src_byte_off
        rol     a

        jsr     set_source
        jsr     set_dest
        copy16  #bitmap_buffer, bits_addr
        ldx     #1
        lda     left_mod14

        sec
        sbc     #7
        bcc     :+
        sta     left_mod14
        dex
:       stx     dhgr_shift_line__offset1_addr
        inx
        stx     dhgr_shift_line__offset2_addr

        lda     left_mod14
        sec
        sbc     bit_offset
        bcs     :+
        adc     #7
        inc     src_width_bytes
        dec     shift_bytes
:       tay                                     ; check if bit shift required
        bne     :+
        ldx     #2*BITS_NO_BITSHIFT
        beq     no_bitshift

:       tya
        asl     a
        tay
        copy16  shift_table_main,y, dhgr_shift_bits__shift_main_addr

        copy16  shift_table_aux,y, dhgr_shift_bits__shift_aux_addr

        ldy     shift_bytes
        sty     dhgr_shift_bits__offset2_addr
        dey
        sty     dhgr_shift_bits__offset1_addr

        ldx     #2*BITS_BITSHIFT
no_bitshift:
        copy16  shift_bits_table,x, dhgr_get_srcbits__shift_bits_jmp_addr
        jmp     bit_blit

BITS_NO_BITSHIFT := 0
BITS_BITSHIFT := 1

;;              BITS_NO_BITSHIFT   BITS_BITSHIFT
shift_bits_table:
        .addr   shift_line_jmp,    dhgr_shift_bits
.endproc


        shift_table_aux := *-2
        .addr   shift_1_aux,shift_2_aux,shift_3_aux
        .addr   shift_4_aux,shift_5_aux,shift_6_aux

        shift_table_main := *-2
        .addr   shift_1_main,shift_2_main,shift_3_main
        .addr   shift_4_main,shift_5_main,shift_6_main


        .include "paintbits-exp.inc"
