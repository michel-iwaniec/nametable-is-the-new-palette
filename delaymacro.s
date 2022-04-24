.macro DELAY arg
.if arg = 2
    nop
.endif
.if arg = 3
    sta delayMacroVar
.endif
.if arg = 4
    nop
    nop
.endif
.if arg = 5
    nop
    sta delayMacroVar
.endif
.if arg = 6
    nop
    nop
    nop
.endif
.if arg = 7
    nop
    nop
    sta delayMacroVar
.endif
.if arg = 8
    nop
    nop
    nop
    nop
.endif
.if arg = 9
    nop
    nop
    nop
    sta delayMacroVar
.endif
.if arg = 10
    nop
    nop
    nop
    nop
    nop
.endif
.if arg = 11
    nop
    nop
    nop
    nop
    sta delayMacroVar
.endif
.if arg = 12
    ;
    inc delayMacroVar
    inc delayMacroVar
    ; 10 cycles
    nop
.endif
.if arg = 13
    ;
    inc delayMacroVar
    inc delayMacroVar
    ; 10 cycles
    sta delayMacroVar
.endif
.if arg = 14
    ;
    inc delayMacroVar
    inc delayMacroVar
    ; 10 cycles
    nop
    nop
.endif
.if arg = 15
    ;
    inc delayMacroVar
    inc delayMacroVar
    ; 10 cycles
    nop
    sta delayMacroVar
.endif
.if arg = 16
    ;
    inc delayMacroVar
    inc delayMacroVar
    ; 10 cycles
    nop
    nop
    nop
.endif
.if arg = 17
    ;
    inc delayMacroVar
    inc delayMacroVar
    ; 10 cycles
    nop
    nop
    sta delayMacroVar
.endif
.if arg >= 18
    ldx #<((arg - 13) / 5)
    jsr Delay5Xplus11
    ; Remainder
    DELAY ((arg - 13) .MOD 5)
.endif
.endmacro

.CODE
;
; Delays by (X-1)*5 + 4 + 7 + 6 = 5X + 11
; Assumes X > 0
;
.align $100
Delay5Xplus11:
:
    dex
    bne :-
    rts
