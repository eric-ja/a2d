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


        MGTK_IMPORT standard_port
        MGTK_IMPORT saved_port
        MGTK_IMPORT apply_port_to_active_port
        MGTK_IMPORT SetPenModeImpl
        MGTK_IMPORT SetPatternImpl
        MGTK_IMPORT SetPortBitsImpl
        MGTK_IMPORT SetFontImpl
        MGTK_IMPORT SetFontImpl__prepare_font

        MGTK_IMPORT MoveImpl
        MGTK_IMPORTZP MoveImpl__params
        MGTK_IMPORT LineImpl
        MGTK_IMPORTZP LineImpl__params
        MGTK_IMPORT LineToImpl
        MGTK_IMPORTZP LineToImpl__params
        MGTK_IMPORT PaintRectImpl
        MGTK_IMPORTZP PaintRectImpl__params
        MGTK_IMPORT FrameRectImpl
        MGTK_IMPORTZP FrameRectImpl__params
        MGTK_IMPORT InRectImpl
        MGTK_IMPORTZP InRectImpl__params
        MGTK_IMPORT PaintBitsImpl
        MGTK_IMPORTZP PaintBitsImpl__params

        MGTK_IMPORT PaintPolyImpl
        MGTK_IMPORT FramePolyImpl
        MGTK_IMPORT InPolyImpl

        MGTK_IMPORT TextWidthImpl
        MGTK_IMPORTZP TextWidthImpl__params
        MGTK_IMPORT DrawTextImpl
        MGTK_IMPORTZP DrawTextImpl__params

        MGTK_IMPORT jt_rts



;;; ============================================================
;;; InitGraf

.proc InitGrafImpl

.if ::VStatus = 'F'
        lda     #$71            ; %0001 lo nibble = HiRes, Page 1, Full, Graphics
.else
        lda     #$41            ; %0001 lo nibble = HiRes, Page 1, Full, Graphics
.endif
        sta     $82             ; (why is high nibble 7 ???)
        jsr     SetSwitchesImpl

        ;; Initialize port
        ldx     #MGTK::grafport_size-1
loop:   lda     standard_port,x
        sta     $8A,x
        sta     current_grafport,x
        dex
        bpl     loop

        ldax    saved_port_addr
        jsr     assign_and_prepare_port

        lda     #$7F
        sta     fill_eor_mask
        jsr     PaintRectImpl
        lda     #$00
        sta     fill_eor_mask
        rts

saved_port_addr:
        .addr   saved_port
.endproc

;;; ============================================================
;;; SetSwitches

;;; 1 byte param, copied to $82

;;; Toggle display softswitches
;;;   bit 0: LoRes if clear, HiRes if set
;;;   bit 1: Page 1 if clear, Page 2 if set
;;;   bit 2: Full screen if clear, split screen if set
;;;   bit 3: Graphics if clear, text if set

.proc SetSwitchesImpl
        param := $82

        lda     DHIRESON        ; enable dhr graphics
        sta     SET80VID

.if ::VStatus < 'B' || (::VStatus = 'B' && ::VRelease < 10)
        ldx     #6
.else
        ldx     #3
.endif
loop:   lsr     param           ; shift low bit into carry
        lda     table,x
        rol     a
        tay                     ; y = table[x] * 2 + carry
        bcs     store

        lda     $C000,y         ; why load vs. store ???
        bcc     :+

store:  sta     $C000,y

:       dex
        bpl     loop
        rts

table:

.if ::VStatus < 'B' || (::VStatus = 'B' && ::VRelease < 10)
        .byte   $80 | <(SET80COL / 2), $80 | <(RAMRDOFF / 2), $80 | <(RAMWRTOFF / 2)
.endif
        .byte   <(TXTCLR / 2), <(MIXCLR / 2), <(LOWSCR / 2), <(LORES / 2)
.endproc

;;; ============================================================
;;; SetPort

.proc SetPortImpl
        ldax    params_addr
        ;; fall through
.endproc

        ;; Call with port address in (X,A)
assign_and_prepare_port:
        stax    active_port
        ;; fall through

        ;; Initializes font (if needed), port, pattern, and fill mode
prepare_port:
        lda     current_textfont+1
        beq     :+              ; only prepare font if necessary
        jsr     SetFontImpl__prepare_font
:       jsr     SetPortBitsImpl
        jsr     SetPatternImpl
        jmp     SetPenModeImpl

;;; ============================================================
;;; GetPort

.proc GetPortImpl
        jsr     apply_port_to_active_port
        ldax    active_port
        ;;  fall through
.endproc

        ;; Store result (X,A) at params
store_xa_at_params:
        ldy     #0

        ;; Store result (X,A) at params+Y
store_xa_at_y:
        sta     (params_addr),y
        txa
        iny
        sta     (params_addr),y
        rts

;;; ============================================================
;;; InitPort

.proc InitPortImpl
        ldy     #MGTK::grafport_size-1 ; Store 36 bytes at params
loop:   lda     standard_port,y
        sta     (params_addr),y
        dey
        bpl     loop
.endproc
rts3:   rts

        ;; ----------------------------------------
        ;; Graphics Primitives

        screen_width := 560
        screen_height := 192

        ;; Initialization
        MGTK_DECL_API  InitGraf,      $01, InitGrafImpl,    0
        MGTK_DECL_API  SetSwitches,   $02, SetSwitchesImpl, 0, ::param, 1

        ;; GrafPort
        MGTK_DECL_API  InitPort,      $03, InitPortImpl,    0
        MGTK_DECL_API  SetPort,       $04, SetPortImpl,     0, current_grafport, MGTK::grafport_size
        MGTK_DECL_API  GetPort,       $05, GetPortImpl,     0
        MGTK_DECL_API  SetPortBits,   $06, SetPortBitsImpl, 0, current_portmap, MGTK::mapinfo_size
        MGTK_DECL_API  SetPenMode,    $07, SetPenModeImpl,  0, current_penmode, 1
        MGTK_DECL_API  SetPattern,    $08, SetPatternImpl,  0, current_penpattern, 8
        MGTK_DECL_API  SetColorMasks, $09, jt_rts,          0, current_colormasks, 2
        MGTK_DECL_API  SetPenSize,    $0a, jt_rts,          0, current_pensize, 2
        MGTK_DECL_API  SetFont,       $0b, SetFontImpl,     0
        MGTK_DECL_API  SetTextBG,     $0c, jt_rts,          0, current_textback, 1

        ;; Drawing
        MGTK_DECL_API  Move,          $0d, MoveImpl,        0, MoveImpl__params, 4
        MGTK_DECL_API  MoveTo,        $0e, jt_rts,          0, current_penloc, 4
        MGTK_DECL_API  Line,          $0f, LineImpl,        1, LineImpl__params, 4
        MGTK_DECL_API  LineTo,        $10, LineToImpl,      1, LineToImpl__params, 4
        MGTK_DECL_API  PaintRect,     $11, PaintRectImpl,   1, PaintRectImpl__params, 8
        MGTK_DECL_API  FrameRect,     $12, FrameRectImpl,   1, FrameRectImpl__params, 8
        MGTK_DECL_API  InRect,        $13, InRectImpl,      0, InRectImpl__params, 8

        MGTK_DECL_API  PaintBits,     $14, PaintBitsImpl,   0, PaintBitsImpl__params, 16

.if ::Variant = 'P'
        ; Graphics Primitives sets the param count for these calls to $ff?
        MGTK_DECL_API  PaintPoly,     $15, PaintPolyImpl,   1, 0, $100
        MGTK_DECL_API  FramePoly,     $16, FramePolyImpl,   1, 0, $100
        MGTK_DECL_API  InPoly,        $17, InPolyImpl,      0, 0, $100
.else
        MGTK_DECL_API  PaintPoly,     $15, PaintPolyImpl,   1
        MGTK_DECL_API  FramePoly,     $16, FramePolyImpl,   1
        MGTK_DECL_API  InPoly,        $17, InPolyImpl,      0
.endif

        ;; Text
        MGTK_DECL_API  TextWidth,     $18, TextWidthImpl,   0, TextWidthImpl__params, 3
        MGTK_DECL_API  DrawText,      $19, DrawTextImpl,    1, DrawTextImpl__params, 3


        MGTK_DECL_ERROR error_in_object         ; $80

.if ::VStatus < 'B' || (::VStatus = 'B' && ::VRelease < 10)
        MGTK_DECL_ERROR error_bad_object        ; $81
        MGTK_DECL_ERROR error_font_too_big      ; $82
        MGTK_DECL_ERROR error_empty_object      ; $83
.else
        MGTK_DECL_ERROR error_empty_object      ; $81
        MGTK_DECL_ERROR error_bad_object        ; $82
        MGTK_DECL_ERROR error_font_too_big      ; $83
.endif

        .segment "MGTK_ERRS"
        .res $90-$84         ; pad to $90


        .include "graf-exp.inc"
