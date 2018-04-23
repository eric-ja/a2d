;;; ============================================================
;;; MouseGraphics ToolKit
;;; ============================================================


.macro exit_call arg
        .if !.def(MGTK__exit_with_a)
        .import MGTK__exit_with_a
        .endif

        lda     #arg
        jmp     MGTK__exit_with_a
.endmacro


.macro MGTK_IMPORT name
        .import .ident( .sprintf("MGTK__%s", .string(name)) )
        name := .ident( .sprintf("MGTK__%s", .string(name)) )
.endmacro

.macro MGTK_IMPORTZP name
        .importzp .ident( .sprintf("MGTK__%s", .string(name)) )
        name := .ident( .sprintf("MGTK__%s", .string(name)) )
.endmacro


.macro MGTK_PARAM_DEFN length, zp, cursor
        .byte zp, ((length) | ((cursor) << 7))
.endmacro


.macro MGTK_DECL_API name, num, impl, cursor, params, paramslen
        .if !.def(__MGTK_JUMPTABLE_RUN__)
            .import __MGTK_JUMPTABLE_RUN__
        .endif

        .pushseg
            .segment "MGTK_JUMPTABLE"

            .if ::VStatus = 'F'
                .assert (__MGTK_JUMPTABLE_RUN__ + num*2) = *, error, .sprintf("Gap in MGTK_DECL_API sequence at $%02x", num)
            .endif

            .ifndef .ident( .sprintf("MGTK__%s", .string(name)) )
                .export .ident( .sprintf("MGTK__%s", .string(name)) ) := (* - __MGTK_JUMPTABLE_RUN__) >> 1
            .endif
        .popseg

        ;; name, num, impl, cursor
        .if .paramcount < 5
            .pushseg
                .segment "MGTK_JUMPTABLE"
                .addr impl

                .segment "MGTK_PARAMTABLE"
                MGTK_PARAM_DEFN  0, 0, cursor
            .popseg
            .exitmacro
        .endif

        .if .match(.left(1, {params}), ::)
            .if .defined(.ident( .sprintf("%s__%s", .string(impl), .string(.mid(1, 10, {params})))))
                MGTK_DECL_API name, num, impl, cursor, .ident( .sprintf("%s__%s", .string(impl), .string(.mid(1, 10, {params})))), paramslen
            .else
                MGTK_DECL_API name, num, impl, cursor, impl params, paramslen
            .endif
            .exitmacro
        .endif

        ;; name, num, impl, params
        .ifblank paramslen
            MGTK_DECL_API name, num, impl, cursor, params, params::size
            .exitmacro
        .endif

        ;; name, num, impl, params, paramslen
        .pushseg
            .segment "MGTK_JUMPTABLE"
            .addr impl

            .segment "MGTK_PARAMTABLE"
            MGTK_PARAM_DEFN  paramslen, params, cursor
        .popseg
.endmacro


.macro MGTK_DECL_ERROR name
        .pushseg
            .segment "MGTK_ERRS"
            .export name := <*
            .res 1
        .popseg
.endmacro
