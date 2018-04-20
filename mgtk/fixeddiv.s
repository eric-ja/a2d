;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================

        .setcpu "6502"

        .include "../mgtk/mgtk.inc"
        .include "../macros.inc"

        .include "mgtk-zp.inc"
        .include "mgtk-macros.inc"

        .segment "MGTK_CODE"


.proc fixed_div
        dividend   := $A1       ; 16.0 format
        divisor    := $A3       ; 16.0 format
        quotient   := $9F       ; 16.16 format
        temp       := $A5

        lda     dividend+1
entry2: ora     dividend
        bne     :+

        sta     quotient
        sta     quotient+1
        sta     dividend
        sta     dividend+1
        beq     done            ; always

:       ldy     #32
        lda     #0
        sta     quotient
        sta     quotient+1
        sta     temp
        sta     temp+1

loop:   asl     quotient
        rol     quotient+1
        rol     dividend
        rol     dividend+1
        rol     temp
        rol     temp+1

        lda     temp
        sec
        sbc     divisor
        tax
        lda     temp+1
        sbc     divisor+1
        bcc     :+
        stx     temp
        sta     temp+1
        inc     quotient
:
        dey
        bne     loop

done:   rts
.endproc

fixed_div2 := fixed_div::entry2

        .include "fixeddiv-exp.inc"
