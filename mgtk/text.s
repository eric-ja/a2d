;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "apple2.inc"
        .include "../inc/apple2.inc"
        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


        .import shift_table_main
        .import shift_table_aux
        .import hires_table_lo, hires_table_hi
        .import adjust_xpos
        .import set_up_fill_mode
        .import set_dest
        .import clip_rect
        .import maybe_stash_low_zp
        .import maybe_unstash_low_zp


;;; ============================================================
;;; SetFont

.define max_font_height 16

.proc SetFontImpl
        copy16  params_addr, current_textfont ; set font to passed address

        ;; Compute addresses of each row of the glyphs.
prepare_font:
        ldy     #0              ; copy first 3 bytes of font defn (type, lastchar, height) to $FD-$FF
:       lda     (current_textfont),y
        sta     $FD,y
        iny
        cpy     #3
        bne     :-

        cmp     #max_font_height+1       ; if height >= 17, skip this next bit
        bcs     end

        ldax    current_textfont
        clc
        adc     #3
        bcc     :+
        inx
:       stax    glyph_widths    ; set $FB/$FC to start of widths

        sec
        adc     glyph_last
        bcc     :+
        inx

:       ldy     #0              ; loop 0... height-1
loop:   sta     glyph_row_lo,y
        pha
        txa
        sta     glyph_row_hi,y
        pla

        sec
        adc     glyph_last
        bcc     :+
        inx

:       bit     glyph_type   ; ($80 = double width, so double the offset)
        bpl     :+

        sec
        adc     glyph_last
        bcc     :+
        inx

:       iny
        cpy     glyph_height_p
        bne     loop
        rts

end:    exit_call MGTK::error_font_too_big
.endproc

glyph_row_lo:
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
glyph_row_hi:
        .byte   $00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$00,$00,$00,$00

;;; ============================================================
;;; TextWidth

;;; 3 bytes of params, copied to $A1

.proc TextWidthImpl
        params := $A1

        jsr     measure_text
        ldy     #3              ; Store result (X,A) at params+3
        sta     (params_addr),y
        txa
        iny
        sta     (params_addr),y
        rts
.endproc

        ;; Call with data at ($A1), length in $A3, result in (X,A)
.proc measure_text
        data   := $A1
        length := $A3

        accum  := $82

        ldx     #0
        ldy     #0
        sty     accum
loop:   sty     accum+1
        lda     (data),y
        tay
        txa
        clc
        adc     (glyph_widths),y
        bcc     :+
        inc     accum
:       tax
        ldy     accum+1
        iny
        cpy     length
        bne     loop
        txa
        ldx     accum
        rts
.endproc

;;; ============================================================

        ;; Turn the current penloc into left, right, top, and bottom.
        ;;
        ;; Inputs:
        ;;    A = width
        ;;    $FF = height
        ;;
.proc penloc_to_bounds
        sec
        sbc     #1
        bcs     :+
        dex
:       clc
        adc     current_penloc_x
        sta     right
        txa
        adc     current_penloc_x+1
        sta     right+1

        copy16  current_penloc_x, left

        lda     current_penloc_y
        sta     bottom
        ldx     current_penloc_y+1
        stx     bottom+1
        clc
        adc     #1
        bcc     :+
        inx
:       sec
        sbc     glyph_height_p
        bcs     :+
        dex
:       stax    top
        rts
.endproc

;;; ============================================================

;;; 3 bytes of params, copied to $A1

.proc DrawTextImpl
        params := $A1

        text_bits_buf := $00
        vid_addrs_table := $20

        shift_aux_ptr := $40
        shift_main_ptr := $42

        blit_mask := $80
        doublewidth_flag := $81

        remaining_width := $9A
        vid_page := $9C
        text_index := $9F
        text_addr := $A1        ; param
        text_len  := $A3        ; param
        text_width := $A4       ; computed


        jsr     maybe_unstash_low_zp
        jsr     measure_text
        stax    text_width

        ldy     #0
        sty     text_index
        sty     $A0
        sty     clipped_left
        sty     clipped_top
        jsr     penloc_to_bounds
        jsr     clip_rect
        bcc     text_clipped

        tya
        ror     a
        bcc     no_left_clip

        ldy     #0
        ldx     vid_page
left_clip_loop:
        sty     text_index
        lda     (text_addr),y
        tay
        lda     (glyph_widths),y
        clc
        adc     clipped_left             ; exit loop when first partially or
        bcc     :+                       ; fully visible glyph is found
        inx
        beq     no_left_clip
:       sta     clipped_left
        ldy     text_index
        iny
        bne     left_clip_loop

no_left_clip:
        jsr     set_up_fill_mode
        jsr     set_dest

        lda     left_mod14
        clc
        adc     clipped_left
        bpl     :+
        inc     width_bytes
        dec     $A0
        adc     #14
:       sta     left_mod14

        lda     width_bytes
        inc     width_bytes
        ldy     current_mapwidth
        bpl     text_clip_ndbm

        ;; For an on-screen destination, width_bytes is set up for the
        ;; pattern blitter, which thinks in terms of double (main & aux)
        ;; transfers. We actually want single transfers here, so we need to
        ;; double it and restore the carry.
        asl     a
        tax
        lda     left_mod14
        cmp     #7
        bcs     :+
        inx
:       lda     right
        beq     :+
        inx
:       stx     width_bytes

text_clip_ndbm:
        lda     left_mod14
        sec
        sbc     #7
        bcc     :+
        sta     left_mod14
:
        lda     #0
        rol     a              ; if left_mod14 was >= 7, then A=1 else A=0
        eor     #1             ; if left_mod14 <7, then A=1 (aux) else A=0 (main)
        sta     vid_page
        tax
        sta     LOWSCR,x       ; set starting page
        jsr     do_draw
        sta     LOWSCR

text_clipped:
        jsr     maybe_stash_low_zp
        ldax    text_width
        jmp     adjust_xpos


do_draw:
        lda     bottom
        sec
        sbc     top
        asl     a
        tax

        ;; Calculate offsets to the draw and blit routines so that they draw
        ;; the exact number of needed lines.
        lda     shifted_draw_line_table,x
        sta     shifted_draw_jmp_addr
        lda     shifted_draw_line_table+1,x
        sta     shifted_draw_jmp_addr+1

        lda     unshifted_draw_line_table,x
        sta     unshifted_draw_jmp_addr
        lda     unshifted_draw_line_table+1,x
        sta     unshifted_draw_jmp_addr+1

        lda     unmasked_blit_line_table,x
        sta     unmasked_blit_jmp_addr
        lda     unmasked_blit_line_table+1,x
        sta     unmasked_blit_jmp_addr+1

        lda     masked_blit_line_table,x
        sta     masked_blit_jmp_addr
        lda     masked_blit_line_table+1,x
        sta     masked_blit_jmp_addr+1

        txa
        lsr     a
        tax
        sec
        stx     $80
        stx     $81
        lda     #0
        sbc     clipped_top
        sta     clipped_top
        tay

        ldx     #(max_font_height-1)*shifted_draw_line_size
        sec

:       lda     glyph_row_lo,y
        sta     shifted_draw_linemax+1,x
        lda     glyph_row_hi,y
        sta     shifted_draw_linemax+2,x
        txa
        sbc     #shifted_draw_line_size
        tax
        iny
        dec     $80
        bpl     :-

        ldy     clipped_top
        ldx     #(max_font_height-1)*unshifted_draw_line_size
        sec
:       lda     glyph_row_lo,y
        sta     unshifted_draw_linemax+1,x
        lda     glyph_row_hi,y
        sta     unshifted_draw_linemax+2,x
        txa
        sbc     #unshifted_draw_line_size
        tax
        iny
        dec     $81
        bpl     :-

        ldy     top
        ldx     #0

        ;; Populate the pointers in vid_addrs_table for the lines we are
        ;; going to be drawing to.
text_dest_loop:
        bit     current_mapwidth
        bmi     text_dest_dhgr

        lda     vid_addr
        clc
        adc     current_mapwidth
        sta     vid_addr
        sta     vid_addrs_table,x

        lda     vid_addr+1
        adc     #0
        sta     vid_addr+1
        sta     vid_addrs_table+1,x
        bne     text_dest_next

text_dest_dhgr:
        lda     hires_table_lo,y
        clc
        adc     left_bytes
        sta     vid_addrs_table,x

        lda     hires_table_hi,y
        ora     current_mapbits+1
        sta     vid_addrs_table+1,x

text_dest_next:
        cpy     bottom
        beq     :+
        iny
        inx
        inx
        bne     text_dest_loop
:

        ldx     #15
        lda     #0
:       sta     text_bits_buf,x
        dex
        bpl     :-
        sta     doublewidth_flag
        sta     shift_aux_ptr               ; zero
        lda     #$80
        sta     shift_main_ptr

        ldy     text_index
next_glyph:
        lda     (text_addr),y
        tay

        bit     doublewidth_flag
        bpl     :+
        sec
        adc     glyph_last
:
        tax
        lda     (glyph_widths),y
        beq     zero_width_glyph

        ldy     left_mod14
        bne     shifted_draw

        ;; Transfer one column of one glyph into the text_bits_buf[0..15]

unshifted_draw_jmp_addr := *+1
        jmp     unshifted_draw_linemax           ; patched to jump into following block


        ;; Unrolled loop from max_font_height-1 down to 0
unshifted_draw_linemax:
        .repeat max_font_height, line
        .ident (.sprintf ("unshifted_draw_line_%d", max_font_height-line-1)):
:       lda     $FFFF,x
        sta     text_bits_buf+max_font_height-line-1

        .ifndef unshifted_draw_line_size
        unshifted_draw_line_size := * - :-
        .else
        .assert unshifted_draw_line_size = * - :-, error, "unshifted_draw_line_size inconsistent"
        .endif

        .endrepeat


zero_width_glyph:
        jmp     do_blit


        ;; Transfer one column of one glyph, shifting it into
        ;; text_bits_buf[0..15] and text_bits_buf[16..31] by left_mod14 bits.

shifted_draw:
        tya
        asl     a
        tay
        copy16  shift_table_aux,y, shift_aux_ptr
        copy16  shift_table_main,y, shift_main_ptr

shifted_draw_jmp_addr := *+1
        jmp     shifted_draw_linemax      ; patched to jump into following block


        ;; Unrolled loop from max_font_height-1 down to 0
shifted_draw_linemax:
        .repeat max_font_height, line
        .ident (.sprintf ("shifted_draw_line_%d", max_font_height-line-1)):

:       ldy     $FFFF,x             ; All of these $FFFFs are modified
        lda     (shift_main_ptr),y
        sta     text_bits_buf+16+max_font_height-line-1
        lda     (shift_aux_ptr),y
        ora     text_bits_buf+max_font_height-line-1
        sta     text_bits_buf+max_font_height-line-1

        .ifndef shifted_draw_line_size
        shifted_draw_line_size := * - :-
        .else
        .assert shifted_draw_line_size = * - :-, error, "shifted_draw_line_size inconsistent"
        .endif

        .endrepeat


do_blit:
        bit     doublewidth_flag
        bpl     :+

        inc     text_index             ; completed a double-width glyph
        lda     #0
        sta     doublewidth_flag
        lda     remaining_width
        bne     advance_x              ; always

:       txa
        tay
        lda     (glyph_widths),y
        cmp     #8
        bcs     :+
        inc     text_index             ; completed a single-width glyph
        bcc     advance_x

:       sbc     #7
        sta     remaining_width
        ror     doublewidth_flag       ; will set to negative
        lda     #7                     ; did the first 7 pixels of a
                                       ; double-width glyph
advance_x:
        clc
        adc     left_mod14
        cmp     #7
        bcs     advance_byte
        sta     left_mod14

L5BFF:  ldy     text_index
        cpy     text_len
        beq     :+
        jmp     next_glyph

:       ldy     $A0
        jmp     last_blit

advance_byte:
        sbc     #7
        sta     left_mod14

        ldy     $A0
        bne     :+
        jmp     first_blit

:       bmi     next_byte
        dec     width_bytes
        bne     unmasked_blit
        jmp     last_blit

unmasked_blit:
unmasked_blit_jmp_addr := *+1
        jmp     unmasked_blit_linemax        ; patched to jump into block below


;;; Per JB: "looks like the quickdraw fast-path draw unclipped pattern slab"

        ;; Unrolled loop from max_font_height-1 down to 0
unmasked_blit_linemax:
        .repeat max_font_height, line
        .ident (.sprintf ("unmasked_blit_line_%d", max_font_height-line-1)):
:       lda     text_bits_buf+max_font_height-line-1
        eor     current_textback
        sta     (vid_addrs_table + 2*(max_font_height-line-1)),y

        .ifndef unmasked_blit_line_size
        unmasked_blit_line_size := * - :-
        .else
        .assert unmasked_blit_line_size = * - :-, error, "unmasked_blit_line_size inconsistent"
        .endif

        .endrepeat


next_byte:
        bit     current_mapwidth
        bpl     text_ndbm

        lda     vid_page
        eor     #1
        tax
        sta     vid_page
        sta     LOWSCR,x
        beq     :+
text_ndbm:
        inc     $A0
:
        ldx     #15
:       lda     text_bits_buf+16,x
        sta     text_bits_buf,x
        dex
        bpl     :-
        jmp     L5BFF


        ;; This is the first (left-most) blit, so it needs masks. If this is
        ;; also the last blit, apply the right mask as well.
first_blit:
        ldx     vid_page
        lda     left_masks_table,x
        dec     width_bytes
        beq     single_byte_blit

        jsr     masked_blit
        jmp     next_byte

single_byte_blit:                      ; a single byte length blit; i.e. start
        and     right_masks_table,x    ; and end bytes are the same
        bne     masked_blit
        rts


        ;; This is the last (right-most) blit, so we have to set up masking.
last_blit:
        ldx     vid_page
        lda     right_masks_table,x
masked_blit:
        ora     #$80
        sta     blit_mask

masked_blit_jmp_addr := *+1
        jmp     masked_blit_linemax


;;; Per JB: "looks like the quickdraw slow-path draw clipped pattern slab"

        ;; Unrolled loop from max_font_height-1 down to 0
masked_blit_linemax:
        .repeat max_font_height, line
        .ident (.sprintf ("masked_blit_line_%d", max_font_height-line-1)):
:       lda     text_bits_buf+max_font_height-line-1
        eor     current_textback
        eor     (vid_addrs_table + 2*(max_font_height-line-1)),y
        and     blit_mask
        eor     (vid_addrs_table + 2*(max_font_height-line-1)),y
        sta     (vid_addrs_table + 2*(max_font_height-line-1)),y

        .ifndef masked_blit_line_size
        masked_blit_line_size := * - :-
        .else
        .assert masked_blit_line_size = * - :-, error, "masked_blit_line_size inconsistent"
        .endif

        .endrepeat

        rts


shifted_draw_line_table:
        .repeat max_font_height, line
        .addr   .ident (.sprintf ("shifted_draw_line_%d", line))
        .endrepeat

unshifted_draw_line_table:
        .repeat max_font_height, line
        .addr   .ident (.sprintf ("unshifted_draw_line_%d", line))
        .endrepeat

unmasked_blit_line_table:
        .repeat max_font_height, line
        .addr   .ident (.sprintf ("unmasked_blit_line_%d", line))
        .endrepeat

masked_blit_line_table:
        .repeat max_font_height, line
        .addr   .ident (.sprintf ("masked_blit_line_%d", line))
        .endrepeat

.endproc

        .include "text-exp.inc"
