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



        .import mouse_operand
        .import mouse_firmware_hi
        .import no_mouse_flag
        .import hires_table_lo, hires_table_hi
        .import kbd_mouse_state
        .import store_xa_at_params
        .import handle_keyboard_mouse
        .import divmod7
        .import mod7_table
        .import shift_table_aux
        .import shift_table_main
        .import use_interrupts



cursor_flag:                    ; high bit clear if cursor drawn, set if not drawn
        .byte   0
cursor_count:
        .byte   $FF             ; decremented on hide, incremented on shown; 0 = visible

.proc set_pos_params
xcoord: .word   0
ycoord: .word   0
.endproc

mouse_state:
mouse_x:        .word   0
mouse_y:        .word   0
mouse_status:   .byte   0       ; bit 7 = is down, bit 6 = was down, still down

mouse_scale_x:  .byte   $00
mouse_scale_y:  .byte   $00

mouse_hooked_flag:              ; High bit set if mouse is "hooked", and calls
        .byte   0               ; bypassed; never appears to be set.

mouse_hook:
        .addr   0

cursor_hotspot_x:  .byte   $00
cursor_hotspot_y:  .byte   $00

cursor_mod7:
        .res    1

cursor_bits:
        .res    3
cursor_mask:
        .res    3

cursor_savebits:
        .res    3*MGTK::cursor_height           ; Saved 3 screen bytes per row.

cursor_data:
        .res    4                               ; Saved values of cursor_char..cursor_y2.

pointer_cursor:
        .byte   px(%0000000),px(%0000000)
        .byte   px(%0100000),px(%0000000)
        .byte   px(%0110000),px(%0000000)
        .byte   px(%0111000),px(%0000000)
        .byte   px(%0111100),px(%0000000)
        .byte   px(%0111110),px(%0000000)
        .byte   px(%0111111),px(%0000000)
        .byte   px(%0101100),px(%0000000)
        .byte   px(%0000110),px(%0000000)
        .byte   px(%0000110),px(%0000000)
        .byte   px(%0000011),px(%0000000)

        .byte   px(%0000000),px(%0000000)
        .byte   px(%1100000),px(%0000000)
        .byte   px(%1110000),px(%0000000)
        .byte   px(%1111000),px(%0000000)
        .byte   px(%1111100),px(%0000000)
        .byte   px(%1111110),px(%0000000)
        .byte   px(%1111111),px(%0000000)
        .byte   px(%1111111),px(%1000000)
        .byte   px(%1111111),px(%0000000)
        .byte   px(%0001111),px(%0000000)
        .byte   px(%0001111),px(%0000000)
        .byte   px(%0000111),px(%1000000)
        .byte   px(%0000111),px(%1000000)

        .byte   1,1

pointer_cursor_addr:
        .addr   pointer_cursor

.proc set_pointer_cursor
        lda     #$FF
        sta     cursor_count
        lda     #0
        sta     cursor_flag
        lda     pointer_cursor_addr
        sta     params_addr
        lda     pointer_cursor_addr+1
        sta     params_addr+1
        ;; fall through
.endproc

;;; ============================================================
;;; SetCursor


.proc SetCursorImpl
        php
        sei
        ldax    params_addr
        stax    active_cursor
        clc
        adc     #MGTK::cursor_offset_mask
        bcc     :+
        inx
:       stax    active_cursor_mask

        ldy     #MGTK::cursor_offset_hotspot
        lda     (params_addr),y
        sta     cursor_hotspot_x
        iny
        lda     (params_addr),y
        sta     cursor_hotspot_y
        jsr     restore_cursor_background
.if ::VStatus < 'B' || (::VStatus = 'B' && ::VRelease < 10)
        jsr     update_cursor
.else
        jsr     draw_cursor
.endif

        plp
.endproc
srts:   rts


        cursor_bytes      := $82
        cursor_softswitch := $83
        cursor_y1         := $84
        cursor_y2         := $85

        vid_ptr           := $88

.proc update_cursor
        lda     cursor_count           ; hidden? if so, skip
        bne     srts
        bit     cursor_flag
        bmi     srts
        ;; Fall-through
.endproc

.proc draw_cursor
        lda     #0
        sta     cursor_count
        sta     cursor_flag

        lda     set_pos_params::ycoord
        clc
        sbc     cursor_hotspot_y
        sta     cursor_y1
        clc
        adc     #MGTK::cursor_height
        sta     cursor_y2

        lda     set_pos_params::xcoord
        sec
        sbc     cursor_hotspot_x
        tax
        lda     set_pos_params::xcoord+1
        sbc     #0
        bpl     :+

        txa                            ; X-coord is negative: X-reg = X-coord + 256
        ror     a                      ; Will shift in zero:  X-reg = X-coord/2 + 128
        tax                            ; Negative mod7 table starts at 252 (since 252%7 = 0), and goes backwards
        ldy     mod7_table+252-128,x   ; Index (X-coord / 2 = X-reg - 128) relative to mod7_table+252
        lda     #$FF                   ; Char index = -1
        bmi     set_divmod

:       jsr     divmod7
set_divmod:
        sta     cursor_bytes            ; char index in line

        tya
        rol     a
        cmp     #7
        bcc     :+
        sbc     #7
:       tay

        lda     #<LOWSCR/2
        rol     a                      ; if mod >= 7, then will be HISCR, else LOWSCR
        eor     #1
        sta     cursor_softswitch      ; $C0xx softswitch index

        sty     cursor_mod7
        tya
        asl     a
        tay
        copy16  shift_table_main,y, cursor_shift_main_addr
        copy16  shift_table_aux,y, cursor_shift_aux_addr

        ldx     #3
:       lda     cursor_bytes,x
        sta     cursor_data,x
        dex
        bpl     :-

        ldx     #$17
        stx     left_bytes
        ldx     #$23
        ldy     cursor_y2
dloop:  cpy     #192
        bcc     :+
        jmp     drnext

:       lda     hires_table_lo,y
        sta     vid_ptr
        lda     hires_table_hi,y
        ora     #$20
        sta     vid_ptr+1
        sty     cursor_y2
        stx     left_mod14

        ldy     left_bytes
        ldx     #$01
:
active_cursor           := * + 1
        lda     $FFFF,y
        sta     cursor_bits,x
active_cursor_mask      := * + 1
        lda     $FFFF,y
        sta     cursor_mask,x
        dey
        dex
        bpl     :-
        lda     #0
        sta     cursor_bits+2
        sta     cursor_mask+2

        ldy     cursor_mod7
        beq     no_shift

        ldy     #5
:       ldx     cursor_bits-1,y

cursor_shift_main_addr           := * + 1
        ora     $FF80,x
        sta     cursor_bits,y

cursor_shift_aux_addr           := * + 1
        lda     $FF00,x
        dey
        bne     :-
        sta     cursor_bits

no_shift:
        ldx     left_mod14
        ldy     cursor_bytes
        lda     cursor_softswitch
        jsr     set_switch
        bcs     :+

        lda     (vid_ptr),y
        sta     cursor_savebits,x

        lda     cursor_mask
        ora     (vid_ptr),y
        eor     cursor_bits
        sta     (vid_ptr),y
        dex
:
        jsr     switch_page
        bcs     :+

        lda     (vid_ptr),y
        sta     cursor_savebits,x
        lda     cursor_mask+1

        ora     (vid_ptr),y
        eor     cursor_bits+1
        sta     (vid_ptr),y
        dex
:
        jsr     switch_page
        bcs     :+

        lda     (vid_ptr),y
        sta     cursor_savebits,x

        lda     cursor_mask+2
        ora     (vid_ptr),y
        eor     cursor_bits+2
        sta     (vid_ptr),y
        dex
:
        ldy     cursor_y2
drnext:
        dec     left_bytes
        dec     left_bytes
        dey
        cpy     cursor_y1
        beq     lowscr_rts
        jmp     dloop
.endproc
drts:   rts

active_cursor        := draw_cursor::active_cursor
active_cursor_mask   := draw_cursor::active_cursor_mask


.proc restore_cursor_background
        lda     cursor_count           ; already hidden?
        bne     drts
        bit     cursor_flag
        bmi     drts

        ldx     #3
:       lda     cursor_data,x
        sta     cursor_bytes,x
        dex
        bpl     :-

        ldx     #$23
        ldy     cursor_y2
cloop:  cpy     #192
        bcs     cnext

        lda     hires_table_lo,y
        sta     vid_ptr
        lda     hires_table_hi,y
        ora     #$20
        sta     vid_ptr+1
        sty     cursor_y2

        ldy     cursor_bytes
        lda     cursor_softswitch
        jsr     set_switch
        bcs     :+
        lda     cursor_savebits,x
        sta     (vid_ptr),y
        dex
:
        jsr     switch_page
        bcs     :+
        lda     cursor_savebits,x
        sta     (vid_ptr),y
        dex
:
        jsr     switch_page
        bcs     :+
        lda     cursor_savebits,x
        sta     (vid_ptr),y
        dex
:
        ldy     cursor_y2
cnext:  dey
        cpy     cursor_y1
        bne     cloop
.endproc
lowscr_rts:
        sta     LOWSCR
        rts


.proc switch_page
        lda     set_switch_sta_addr
        eor     #1
        cmp     #<LOWSCR
        beq     set_switch
        iny
        ;; Fall through
.endproc

.proc set_switch
        sta     switch_sta_addr
switch_sta_addr := *+1
        sta     $C0FF
        cpy     #$28
        rts
.endproc

set_switch_sta_addr := set_switch::switch_sta_addr


;;; ============================================================
;;; ShowCursor

.proc ShowCursorImpl
        php
        sei
        lda     cursor_count
        beq     done
        inc     cursor_count
        bmi     done
        beq     :+
        dec     cursor_count
:       bit     cursor_flag
        bmi     done
        jsr     draw_cursor
done:   plp
        rts
.endproc

;;; ============================================================
;;; ObscureCursor

.proc ObscureCursorImpl
        php
        sei
        jsr     restore_cursor_background
        lda     #$80
        sta     cursor_flag
        plp
        rts
.endproc

;;; ============================================================
;;; HideCursor

.proc HideCursorImpl
        php
        sei
        jsr     restore_cursor_background
        dec     cursor_count
        plp
.endproc
mrts:   rts

;;; ============================================================

cursor_throttle:
        .byte   0

.proc move_cursor
        bit     use_interrupts
        bpl     :+

        lda     kbd_mouse_state
        bne     :+
        dec     cursor_throttle
        lda     cursor_throttle
        bpl     mrts
        lda     #2
        sta     cursor_throttle

:       ldx     #2
:       lda     mouse_x,x
        cmp     set_pos_params,x
        bne     mouse_moved
        dex
        bpl     :-
        bmi     no_move

mouse_moved:
        jsr     restore_cursor_background
        ldx     #2
        stx     cursor_flag
:       lda     mouse_x,x
        sta     set_pos_params,x
        dex
        bpl     :-
        jsr     update_cursor

no_move:
        bit     no_mouse_flag
        bmi     :+
        jsr     read_mouse_pos

:       bit     no_mouse_flag
        bpl     :+
        lda     #0
        sta     mouse_status

:       lda     kbd_mouse_state
        beq     rts4
        jsr     handle_keyboard_mouse
.endproc
rts4:   rts

;;; ============================================================

.proc read_mouse_pos
        ldy     #READMOUSE
        jsr     call_mouse
        bit     mouse_hooked_flag
        bmi     do_scale_x

        ldx     mouse_firmware_hi
        lda     MOUSE_X_LO,x
        sta     mouse_x
        lda     MOUSE_X_HI,x
        sta     mouse_x+1
        lda     MOUSE_Y_LO,x
        sta     mouse_y

        ;; Scale X
do_scale_x:
        ldy     mouse_scale_x
        beq     do_scale_y
:       lda     mouse_x
        asl     a
        sta     mouse_x
        lda     mouse_x+1
        rol     a
        sta     mouse_x+1
        dey
        bne     :-

        ;; Scale Y
do_scale_y:
        ldy     mouse_scale_y
        beq     done_scaling
        lda     mouse_y
:       asl     a
        dey
        bne     :-
        sta     mouse_y

done_scaling:
        bit     mouse_hooked_flag
        bmi     done
        lda     MOUSE_STATUS,x
        sta     mouse_status
done:   rts
.endproc

;;; ============================================================
;;; GetCursorAddr

.proc GetCursorAddrImpl
        ldax    active_cursor
        jmp     store_xa_at_params
.endproc

;;; ============================================================

        ;; Call mouse firmware, operation in Y, param in A
.proc call_mouse
        proc_ptr          := $88

        bit     no_mouse_flag
        bmi     rts4

        bit     mouse_hooked_flag
        bmi     hooked
        pha
        ldx     mouse_firmware_hi
        stx     proc_ptr+1
        lda     #$00
        sta     proc_ptr
        lda     (proc_ptr),y
        sta     proc_ptr
        pla
        ldy     mouse_operand
        jmp     (proc_ptr)

hooked: jmp     (mouse_hook)
.endproc

        .include "cursor-exp.inc"

