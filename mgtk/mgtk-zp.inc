;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

;;; ============================================================
;;; ZP Usage

        params_addr               := $80

        vid_addr                  := $84  ; pointer to video memory
        left_bytes                := $86  ; offset of leftmost coordinate in chars (0-39)
        left_mod14                := $87  ; starting x-coordinate mod 14
        bits_addr                 := $8E  ; pointer to pattern/bitmap

        width_bytes               := $91  ; width of rectangle in chars
        left_masks_table          := $92  ; bitmasks for left edge indexed by page (0=main, 1=aux)
        right_masks_table         := $96  ; bitmasks for right edge indexed by page (0=main, 1=aux)

        clipped_left              := $9B  ; number of bits clipped off left side
        clipped_top               := $9D  ; number of bits clipped off top side

        left                      := $92
        top                       := $94  ; top/starting/current y-coordinate
        bottom                    := $98  ; bottom/ending/maximum y-coordinate
        right                     := $96

        current_grafport          := $D0
        current_portmap           := $D0
        current_viewloc_x         := $D0
        current_viewloc_y         := $D2
        current_mapbits           := $D4
        current_mapwidth          := $D6
        current_maprect_x1        := $D8
        current_maprect_y1        := $DA
        current_maprect_x2        := $DC
        current_maprect_y2        := $DE
        current_penpattern        := $E0
        current_colormasks        := $E8
        current_colormask_and     := $E8
        current_colormasks_or     := $E9
        current_penloc            := $EA
        current_penloc_x          := $EA
        current_penloc_y          := $EC
        current_pensize           := $EE
        current_penwidth          := $EE
        current_penheight         := $EF
        current_penmode           := $F0
        current_textback          := $F1
        current_textfont          := $F2

        active_port               := $F4  ; address of live port block

        fill_eor_mask             := $F6
        x_offset                  := $F7
        y_offset                  := $F9

        glyph_widths              := $FB  ; address
        glyph_type                := $FD  ; 0=regular, $80=double width
        glyph_last                := $FE  ; last glyph index
        glyph_height_p            := $FF  ; glyph height
