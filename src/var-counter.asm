; variable access counter routine
;
; SYS49152 to activate, then NEW in BASIC
; SYS49502 to display variable access counts

vartab2ptr = $fb

*=$c000

; copy BASIC ROM to RAM
    sei 
    lda #$a0
loop0:
    sta $03
    ldy #$00
    sty $02
loop1:
    lda ($02),Y
    sta ($02),Y
    dey
    bne loop1
    inc $03
    lda $03
    beq ahead
    cmp #$c0
    bne loop1
    lda #$e0
    bne loop0

ahead:
do_patch:
    lda #$4c            ; replaces:
    sta $b0db           ; $B0DB  STX $46
    lda #$00            ; $B0DD  SEC
    sta $b0dc
    lda #$C1            ; ...with
    sta $b0dd           ; JMP $C100

    LDA #$36            ; disable basic rom
    STA $01
    CLI                 ; Enable interrupts

    lda #$00            ; empty the vart2table
    sta $c200

    RTS                 ; Return from subroutine

    ; basic patch when accessing a variable
*=$C100
    stx $46             ; do normally executed code
    sec

    php                 ; save registers on stack
    pha                 
    txa
    pha
    tya
    pha

    lda #$00            ; set and clear our temp variable info block
    sta vartab2ptr
    lda #$C2
    sta vartab2ptr+1

    ldy #$00
searchloop:         
    lda (vartab2ptr),y  ; get 1st value in vartab2
    beq notfound        ; if zero, no more variables
    cmp $45             ; compare it with the first character in variable name
    bne nextvar
    iny
    lda (vartab2ptr), y ; get 2nd value in vartab2
    cmp $46             ; compare it to second character in variable name
    bne nextvar+1
    iny
    iny                 ; skip ahead twice since count is 16 bit value
    clc
    lda (vartab2ptr),y  ; get the lo-byte counter for this variable
    adc #$01            ; add 1 to it
    sta (vartab2ptr),y  ; save it back
    dey                 ; back up the counter to the hi byte
    lda (vartab2ptr),y  ; get the hi-byte counter for this variable
    adc #$00            ; add any carry
    sta (vartab2ptr),y 
    iny
    jmp done            ; finished, exit

nextvar:
    iny
    iny
    iny
    iny
    jmp searchloop

notfound:
    lda $45
    sta (vartab2ptr),y
    iny
    lda $46
    sta (vartab2ptr),y
    iny
    lda #$00
    sta (vartab2ptr),y  ; save the new variable, with value of 1 (0=low byte)
    iny
    lda #$01
    sta (vartab2ptr),y  ; save the new variable, with value of 1
    iny
    lda #$00
    sta (vartab2ptr),y  ; zero indicates end


done:
    pla
    tay 
    pla 
    tax 
    pla
    plp
    jmp $b0de           ; jump back to normal code

; print values
print_counts:
    lda #$00
    sta vartab2ptr
    lda #$C2
    sta vartab2ptr+1

    ldy #$00
printmsg_loop:
    lda msg, y
    beq + 
    jsr $ffd2
    iny
    jmp printmsg_loop

+
    ldy #$00
printvarloop:         
    lda (vartab2ptr),y  ; get 1st value in vartab2
    beq print_done      ; if zero, no more variables
    jsr $ffd2
    iny
    lda (vartab2ptr), y ; get 2nd value in vartab2
    beq +
    jsr $ffd2
+   lda #'='
    jsr $ffd2
    iny
    iny
    
    tya                 ; stash the counter (bdcd may trash the Y reg)
    pha

    lda (vartab2ptr),y  ; get the counter for this variable
    tax                 ; transfer low byte to X
    dey                 ; back up
    lda (vartab2ptr),Y  ; get high byte in A
    jsr $bdcd           ; print the decimal value of X,A
    
    lda #$0d
    jsr $ffd2
    
    pla                 ; get the counter back
    tay
    iny                 ; increase counter for next variable
    jmp printvarloop
    
    
print_done:
    rts

msg:
.text "variable accesses:", $0d, $00