;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "apple2.inc"
        .include "../mgtk/mgtk.inc"
        .include "../desktop.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"

        .segment "MGTK_CODE"


;;; ============================================================
;;; Routines called during PaintRect etc based on
;;; current_penmode

        ;; ZP usage
        src_addr             := $82        ; pointer to source bitmap
        left_sidemask        := $88        ; bitmask applied to clip left edge of rect
        right_sidemask       := $89        ; bitmask applied to clip right edge of rect
        src_y_coord          := $8C

        src_mapwidth         := $90        ; source stride; $80 = DHGR layout

        fixed_div_dividend   := $A1        ; parameters used by fixed_div proc
        fixed_div_divisor    := $A3
        fixed_div_quotient   := $9F        ; fixed 16.16 format

        ;; Text page usage (main/aux)
        pattern_buffer  := $0400        ; buffer for currently selected pattern (page-aligned)
        bitmap_buffer   := $0601        ; scratchpad area for drawing bitmaps/patterns


        .import mod7_table
        .import div7_table
        .import hires_table_lo
        .import hires_table_hi
        .import shift_1_aux
        .import shift_1_main
        .import src_width_bytes


;;; ============================================================
;;; Fill/blit core

.proc fillmode_copy
        lda     (vid_addr),y
        eor     (bits_addr),y
        eor     fill_eor_mask
        and     right_sidemask
        eor     (vid_addr),y
        bcc     :+
loop:   lda     (bits_addr),y
        eor     fill_eor_mask
:       and     current_colormask_and
        ora     current_colormasks_or
        sta     (vid_addr),y
        dey
        bne     loop
.endproc
.proc fillmode_copy_onechar
        lda     (vid_addr),y
        eor     (bits_addr),y
        eor     fill_eor_mask
        and     left_sidemask
        eor     (vid_addr),y
        and     current_colormask_and
        ora     current_colormasks_or
        sta     (vid_addr),y
        rts
.endproc

.proc fillmode_or
        lda     (bits_addr),y
        eor     fill_eor_mask
        and     right_sidemask
        bcc     :+
loop:   lda     (bits_addr),y
        eor     fill_eor_mask
:       ora     (vid_addr),y
        and     current_colormask_and
        ora     current_colormasks_or
        sta     (vid_addr),y
        dey
        bne     loop
.endproc
.proc fillmode_or_onechar
        lda     (bits_addr),y
        eor     fill_eor_mask
        and     left_sidemask
        ora     (vid_addr),y
        and     current_colormask_and
        ora     current_colormasks_or
        sta     (vid_addr),y
        rts
.endproc

.proc fillmode2_xor
        lda     (bits_addr),y
        eor     fill_eor_mask
        and     right_sidemask
        bcc     :+
loop:   lda     (bits_addr),y
        eor     fill_eor_mask
:       eor     (vid_addr),y
        and     current_colormask_and
        ora     current_colormasks_or
        sta     (vid_addr),y
        dey
        bne     loop
.endproc
.proc fillmode2_xor_onechar
        lda     (bits_addr),y
        eor     fill_eor_mask
        and     left_sidemask
        eor     (vid_addr),y
        and     current_colormask_and
        ora     current_colormasks_or
        sta     (vid_addr),y
        rts
.endproc

.proc fillmode_bic
        lda     (bits_addr),y
        eor     fill_eor_mask
        and     right_sidemask
        bcc     :+
loop:   lda     (bits_addr),y
        eor     fill_eor_mask
:       eor     #$FF
        and     (vid_addr),y
        and     current_colormask_and
        ora     current_colormasks_or
        sta     (vid_addr),y
        dey
        bne     loop
.endproc
.proc fillmode_bic_onechar
        lda     (bits_addr),y
        eor     fill_eor_mask
        and     left_sidemask
        eor     #$FF
        and     (vid_addr),y
        and     current_colormask_and
        ora     current_colormasks_or
        sta     (vid_addr),y
        rts
.endproc


        ;; Main fill loop.

.proc fill_next_line
        cpx     bottom                  ; fill done?
        beq     :+
        inx

get_srcbits_jmp:
get_srcbits_jmp_addr := *+1
        jmp     start_fill_jmp          ; patched to *_get_srcbits if there
                                        ; is a source bitmap
:       rts
.endproc

        ;; Copy a line of source data from a non-display bitmap buffer to
        ;; the staging buffer at $0601.

.proc ndbm_get_srcbits
        lda     load_addr
        adc     src_mapwidth
        sta     load_addr
        bcc     :+
        inc     load_addr+1

:       ldy     src_width_bytes

loop:
load_addr       := *+1
        lda     $FFFF,y                 ; off-screen BMP will be patched here
        and     #$7F
        sta     bitmap_buffer,y
        dey
        bpl     loop
        bmi     shift_bits_clc_jmp
.endproc

        ;; Copy a line of source data from the DHGR screen to the staging
        ;; buffer at $0601.

.proc dhgr_get_srcbits
        index         := $81
        src_byte_off  := $8A        ; char offset within source line

        ldy     src_y_coord
        inc     src_y_coord
        lda     hires_table_hi,y
        ora     $80
        sta     src_addr+1
        lda     hires_table_lo,y
        adc     src_byte_off
        sta     src_addr

get_bits:
        stx     index
        ldy     #0
        ldx     #0
loop:   sta     HISCR
        lda     (src_addr),y
        and     #$7F
        sta     LOWSCR

offset1_addr    := *+1
        sta     bitmap_buffer,x
        lda     (src_addr),y
        and     #$7F

offset2_addr    := *+1
        sta     bitmap_buffer+1,x
        iny
        inx
        inx
        cpx     src_width_bytes
        bcc     loop
        beq     loop
        ldx     index

shift_bits_clc_jmp:
        clc

shift_bits_jmp:
shift_bits_jmp_addr := *+1
        jmp     shift_line_jmp          ; patched to dhgr_shift_bits when needed
.endproc


shift_bits_clc_jmp := dhgr_get_srcbits::shift_bits_clc_jmp


        ;; Subprocedure used to shift bitmap data by a number of bits.

.proc dhgr_shift_bits
        index   := $82

        stx     index
        ldy     src_width_bytes
        lda     #$00
loop:   ldx     bitmap_buffer,y

shift_main_addr := *+1
        ora     shift_1_main,x
offset2_addr := *+1
        sta     bitmap_buffer+1,y
shift_aux_addr := *+1
        lda     shift_1_aux,x
        dey
        bpl     loop
offset1_addr := *+1
        sta     bitmap_buffer
        ldx     index

shift_line_jmp:
shift_line_jmp_addr := *+1
        jmp     dhgr_next_line          ; patched to dhgr_shift_line when needed
.endproc


shift_line_jmp := dhgr_shift_bits::shift_line_jmp


        ;; Subprocedure used to shift bitmap data by an integral number of
        ;; chars.

.proc dhgr_shift_line
        index   := $82

        stx     index
        ldx     #0
        ldy     #0
loop:
offset1_addr := *+1
        lda     bitmap_buffer,x
        sta     HISCR
        sta     bitmap_buffer,y
        sta     LOWSCR

offset2_addr := *+1
        lda     bitmap_buffer+1,x
        sta     bitmap_buffer,y
        inx
        inx
        iny
        cpy     width_bytes
        bcc     loop
        beq     loop

        ldx     index
        jmp     dhgr_next_line
.endproc


        ;; Entry point to start bit blit operation.

.proc bit_blit
        ldx     top
        clc
        jmp     fill_next_line::get_srcbits_jmp
.endproc


        ;; Entry point to start fill after fill mode and destination have
        ;; been set.

.proc do_fill
        ldx     no_srcbits_addr                         ; Disable srcbits fetching
        stx     fill_next_line::get_srcbits_jmp_addr    ; for fill operation.
        ldx     no_srcbits_addr+1
        stx     fill_next_line::get_srcbits_jmp_addr+1

        ldx     top
        ;; Fall-through
.endproc

start_fill_jmp:
start_fill_jmp_addr := *+1
        jmp     dhgr_start_fill         ; patched to *_start_fill


        ;; Start a fill targeting a non-display bitmap (NDBM)

.proc ndbm_start_fill
        txa                     ; pattern y-offset
        ror     a
        ror     a
        ror     a
        and     #$C0            ; to high 2 bits
        ora     left_bytes
        sta     src_addr

        lda     #>pattern_buffer
        adc     #0
        sta     src_addr+1
        jmp     dhgr_get_srcbits::get_bits
.endproc


        ;; Start a fill targeting the DHGR screen.

.proc dhgr_start_fill
        txa                     ; pattern y-offset
        ror     a
        ror     a
        ror     a
        and     #$C0            ; to high 2 bits
        ora     left_bytes
        sta     bits_addr

        lda     #>pattern_buffer
        adc     #0
        sta     bits_addr+1

next_line_jmp_addr := *+1
        jmp     dhgr_next_line
.endproc


        ;; Advance to the next line and fill (non-display bitmap
        ;; destination.)

.proc ndbm_next_line
        lda     vid_addr
        clc
        adc     current_mapwidth
        sta     vid_addr
        bcc     :+
        inc     vid_addr+1
        clc
:       ldy     width_bytes

        jsr     fillmode_jmp
        jmp     fill_next_line
.endproc


        ;; Set vid_addr for the next line and fill (DHGR destination.)

.proc dhgr_next_line
        lda     hires_table_hi,x
        ora     current_mapbits+1
        sta     vid_addr+1
        lda     hires_table_lo,x
        clc
        adc     left_bytes
        sta     vid_addr

        ldy     #1                      ; aux mem
        jsr     dhgr_fill_line
        ldy     #0                      ; main mem
        jsr     dhgr_fill_line
        jmp     fill_next_line
.endproc


        ;; Fill one line in either main or aux screen memory.

.proc dhgr_fill_line
        sta     LOWSCR,y

        lda     left_masks_table,y
        ora     #$80
        sta     left_sidemask

        lda     right_masks_table,y
        ora     #$80
        sta     right_sidemask

        ldy     width_bytes
        ;; Fall-through
.endproc

fillmode_jmp:
        jmp     fillmode_copy       ; modified with fillmode routine

        ;; Address of jump used when drawing from a pattern rather than
        ;; source data bits.
no_srcbits_addr:
        .addr   start_fill_jmp

main_right_masks:
        .byte   $00,$00,$00,$00,$00,$00,$00
aux_right_masks:
        .byte   $01,$03,$07,$0F,$1F,$3F,$7F

main_left_masks:
        .byte   $7F,$7F,$7F,$7F,$7F,$7F,$7F
aux_left_masks:
        .byte   $7F,$7E,$7C,$78,$70,$60,$40
        .byte   $00,$00,$00,$00,$00,$00,$00


        ;; Tables used for fill modes

        ; Fill routines that handle >1 char between left and right limits.
fill_mode_table:
        .addr   fillmode_copy,fillmode_or,fillmode2_xor,fillmode_bic
        .addr   fillmode_copy,fillmode_or,fillmode2_xor,fillmode_bic

        ; Fill routines that handle only 1 char.
fill_mode_table_onechar:
        .addr   fillmode_copy_onechar,fillmode_or_onechar,fillmode2_xor_onechar,fillmode_bic_onechar
        .addr   fillmode_copy_onechar,fillmode_or_onechar,fillmode2_xor_onechar,fillmode_bic_onechar

;;; ============================================================
;;; SetPenMode

.proc SetPenModeImpl
        lda     current_penmode
        ldx     #0
        cmp     #4
        bcc     :+
        ldx     #$7F
:       stx     fill_eor_mask
        rts
.endproc

        ;; Called from PaintRect, DrawText, etc to configure
        ;; fill routines from mode.

.proc set_up_fill_mode
        x1      := $92
        x2      := $96

        x1_bytes := $86
        x2_bytes := $82

        add16   x_offset, x2, x2
        add16   y_offset, bottom, bottom

        add16   x_offset, x1, x1
        add16   y_offset, top, top

        lsr     x2+1
        beq     :+
        jmp     rl_ge256

:       lda     x2
        ror     a
        tax
        lda     div7_table,x
        ldy     mod7_table,x

set_x2_bytes:
        sta     x2_bytes
        tya
        rol     a
        tay
        lda     aux_right_masks,y
        sta     right_masks_table+1
        lda     main_right_masks,y
        sta     right_masks_table

        lsr     x1+1
        bne     ll_ge256
        lda     x1
        ror     a
        tax
        lda     div7_table,x
        ldy     mod7_table,x

set_x1_bytes:
        sta     x1_bytes
        tya
        rol     a
        tay
        sty     left_mod14
        lda     aux_left_masks,y
        sta     left_masks_table+1
        lda     main_left_masks,y
        sta     left_masks_table
        lda     x2_bytes
        sec
        sbc     x1_bytes

set_width:                                      ; Set width for destination.
        sta     width_bytes
        pha
        lda     current_penmode
        asl     a
        tax
        pla
        bne     :+                              ; Check if one or more than one is needed

        lda     left_masks_table+1              ; Only one char is needed, so combine
        and     right_masks_table+1             ; the left and right masks and use the
        sta     left_masks_table+1              ; one-char fill subroutine.
        sta     right_masks_table+1
        lda     left_masks_table
        and     right_masks_table
        sta     left_masks_table
        sta     right_masks_table

        copy16  fill_mode_table_onechar,x, fillmode_jmp+1
        rts

:       copy16  fill_mode_table,x, fillmode_jmp+1
        rts

ll_ge256:                               ; Divmod for left limit >= 256
        lda     x1
        ror     a
        tax
        php
        lda     div7_table+4,x
        clc
        adc     #$24
        plp
        ldy     mod7_table+4,x
        bpl     set_x1_bytes

rl_ge256:                               ; Divmod for right limit >= 256
        lda     x2
        ror     a
        tax
        php
        lda     div7_table+4,x
        clc
        adc     #$24
        plp
        ldy     mod7_table+4,x
        bmi     divmod7
        jmp     set_x2_bytes
.endproc


.proc divmod7
        lsr     a
        bne     :+
        txa
        ror     a
        tax
        lda     div7_table,x
        ldy     mod7_table,x
        rts

:       txa
        ror     a
        tax
        php
        lda     div7_table+4,x
        clc
        adc     #$24
        plp
        ldy     mod7_table+4,x
        rts
.endproc


        ;; Set up destination (for either on-screen or off-screen bitmap.)

.proc set_dest
        DEST_NDBM       := 0            ; draw to off-screen bitmap
        DEST_DHGR       := 1            ; draw to DHGR screen

        lda     left_bytes
        ldx     top
        ldy     current_mapwidth
        jsr     ndbm_calc_dest
        clc
        adc     current_mapbits
        sta     vid_addr
        tya
        adc     current_mapbits+1
        sta     vid_addr+1

        lda     #2*DEST_DHGR
        tax
        tay
        bit     current_mapwidth
        bmi     on_screen               ; negative for on-screen destination

        copy16  #bitmap_buffer, bits_addr

        jsr     ndbm_fix_width
        txa
        inx
        stx     src_width_bytes
        jsr     set_up_fill_mode::set_width

        copy16  shift_line_jmp_addr, dhgr_get_srcbits::shift_bits_jmp_addr
        lda     #2*DEST_NDBM
        ldx     #2*DEST_NDBM
        ldy     #2*DEST_NDBM

on_screen:
        pha
        lda     next_line_table,x
        sta     dhgr_start_fill::next_line_jmp_addr
        lda     next_line_table+1,x
        sta     dhgr_start_fill::next_line_jmp_addr+1
        pla
        tax
        copy16  start_fill_table,x, start_fill_jmp+1
        copy16  shift_line_table,y, dhgr_shift_bits::shift_line_jmp_addr
        rts
.endproc


        ;; Fix up the width and masks for an off-screen destination,

ndbm_fix_width:
        lda     width_bytes
        asl     a
        tax
        inx

        lda     left_masks_table+1
        bne     :+
        dex
        inc     bits_addr
        inc16   vid_addr
        lda     left_masks_table
:       sta     left_sidemask

        lda     right_masks_table
        bne     :+
        dex
        lda     right_masks_table+1
:       sta     right_sidemask
        rts

;;              DEST_NDBM        DEST_DHGR
shift_line_jmp_addr:
        .addr   shift_line_jmp

start_fill_table:
        .addr   ndbm_start_fill, dhgr_start_fill
next_line_table:
        .addr   ndbm_next_line,  dhgr_next_line
shift_line_table:
        .addr   ndbm_next_line,  dhgr_shift_line


        ;; Set source for bitmap transfer (either on-screen or off-screen bitmap.)

.proc set_source
        SRC_NDBM        := 0
        SRC_DHGR        := 1

        ldx     src_y_coord
        ldy     src_mapwidth

.if ::VStatus > 'B' || (::VStatus = 'B' && ::VRelease >= 10)
        bmi     :+
        jsr     mult_x_y
:
.else
        jsr     ndbm_calc_dest
.endif
        clc
        adc     bits_addr
        sta     ndbm_get_srcbits::load_addr
        tya
        adc     bits_addr+1
        sta     ndbm_get_srcbits::load_addr+1

        ldx     #2*SRC_DHGR
        bit     src_mapwidth
        bmi     :+

        ldx     #2*SRC_NDBM
:       copy16  get_srcbits_table,x, fill_next_line::get_srcbits_jmp_addr
        rts

;;              SRC_NDBM           SRC_DHGR
get_srcbits_table:
        .addr   ndbm_get_srcbits,  dhgr_get_srcbits
.endproc


        ;; Calculate destination for off-screen bitmap.

.proc ndbm_calc_dest
        bmi     on_screen        ; do nothing for on-screen destination
        asl     a

mult_x_y:
        stx     $82
        sty     $83
        ldx     #8
loop:   lsr     $83
        bcc     :+
        clc
        adc     $82
:       ror     a
        ror     vid_addr
        dex
        bne     loop

        sty     $82
        tay
        lda     vid_addr
        sec
        sbc     $82
        bcs     on_screen
        dey
on_screen:
        rts
.endproc


mult_x_y := ndbm_calc_dest::mult_x_y


;;; ============================================================
;;; SetPattern

;; Expands the pattern to 8 rows of DHGR-style bitmaps at
;; $0400, $0440, $0480, $04C0, $0500, $0540, $0580, $05C0
;; (using both main and aux mem.)

.proc SetPatternImpl
        lda     #<pattern_buffer
        sta     bits_addr

        lda     y_offset
        and     #7
        lsr     a
        ror     bits_addr
        lsr     a
        ror     bits_addr
        adc     #>pattern_buffer
        sta     bits_addr+1

        ldx     #7
loop:   lda     x_offset
        and     #7
        tay

        lda     current_penpattern,x
:       dey
        bmi     :+
        cmp     #$80
        rol     a
        bne     :-

:       ldy     #$27
:       pha
        lsr     a
        sta     LOWSCR
        sta     (bits_addr),y
        pla
        ror     a
        pha
        lsr     a
        sta     HISCR
        sta     (bits_addr),y
        pla
        ror     a
        dey
        bpl     :-

        lda     bits_addr
        sec
        sbc     #$40
        sta     bits_addr
        bcs     next

        ldy     bits_addr+1
        dey
        cpy     #>pattern_buffer
        bcs     :+
        ldy     #>pattern_buffer+1
:       sty     bits_addr+1

next:   dex
        bpl     loop
        sta     LOWSCR
        rts
.endproc

        .include "fillblit-exp.inc"
