/*
  tinyTwofish implementation for tinyAVR (ATtiny25/45/85)

  Copyright (C) 2014 Marc Schoolderman

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

*/

.altmacro

.include "2fish_avr.cfg"
.include "avrmacros.s"

.global twofish_init, twofish_key, twofish_enc

.text 512

.macro pht a, b
;.print "pht a, b"
    addq a, b
    addq b, a
.endm

MDS_POLY = 0x169

/* poly must be pre-shifted */
.macro gf_shr reg, poly
;.print "gf_shr reg, poly"
local skip
    lsr reg
    brcc skip
    eor reg, poly
skip:
.endm

/* perform sbox lookup & (optional) copy */
.macro sxlati d, r=n/a
local i
i=0
.irp j, 0,0,0,2
    .ifc <r>, <n/a>
    mov r30, d+i
    .else
    mov r30, ((r)&~3)+((r)+i)%4
    .endif
    lpm d+i, Z
    subi r31, 2*j-1
    i=i+1
.endr
.endm

.macro mds_columni dst, src, poly, coef, op=eor
local i
    quad op dst, src
.irp val, 0xEF,0x5B
    gf_shr src, poly
i=0
.irp x, coef
    .if x >= val
    eor dst+i, src
    .endif
    i=i+1
.endr
.endr
.endm

/* select meaning: (hi-bit:lo-bit) =>
   00 = 0x01
   01 = 0x5B
   11 = 0xEF */
.macro mds_column dst, src, poly, coef ; trying to 'roll' this barely gains anything
;.print "mds_column dst, src, poly, coef"
    quad eor dst, src
    .irp val, 4,0
    gf_shr src, poly
    .irp i, 0,1,2,3
    sbrc coef, val+i
    eor dst+i, src
    .endr
    .endr
.endm

.macro round_h dst, src, step=<8+0>, tmpw=
;.print "round_h dst, src, step, tmpw"
#? Y -> key material
    sxlati dst, src                 ; round_h simply a lookup in this case
.endm

.macro round_g_init force=0
.if UNROLL_round_g || force
    ldi r31, hi8(twofish_sbox)
.endif
.endm

/* uses r0..r3 as working area */
.macro round_g out, src, step=<4+0>
;.print "round_g out, src, step"
#? Y -> key material
local i, loop
.if UNROLL_round_g
    round_h 0, src, step, out
    ldi r30, MDS_POLY>>1
    mds_columni out, r0, r30, <0x01,0x5B,0xEF,0xEF>, mov
    mds_columni out, r1, r30, <0xEF,0xEF,0x5B,0x01>
    mds_columni out, r2, r30, <0x5B,0xEF,0x01,0xEF>
    mds_columni out, r3, r30, <0x5B,0x01,0xEF,0x5B>
.else
    round_g_init 1
    round_h 0, src, step, out

    push Y_L ;( we are out of registers.
    ; an encoding of the MDS matrix
    .irp controlword, 0b01001101, 0b10101011, 0b00110111, 0b11001110
    ldi r30, controlword
    push r30
    .endr
    quad clr out
    ldi Y_L, MDS_POLY>>1
    clr r30      ; uze Z to access the register file     
loop:
    clr r31
    ld r0, Z+
    pop r31
    mds_column out, r0, Y_L, r31
    cpi r30, 4
    brlo loop
    pop Y_L
.endif
.endm

/* Necessary to prevent instantiating round_g twice in round_F
 * (using the 'shared' macro)
 * (see below)
 */
.macro round_g_rot out, src
;.print "round_g_rot out, src"
    ; assume the caller set up the stride in 'out'
    round_g out, src, out
    xchgq src, src+7, 0
.endm

/*
register allocation plan:
 r0.. r3: work area (for MDS in particular)
 r4.. r7,r8..r11: feistel half
 r12..       r19: feistel half
 r20..       r27: temporary

round-keys: add in from memory; there is not enough room, and saving
one of the feistel half isn't any better. 

Y -> running key
Z -> in principal, used by sbox/qbox. 

 */

/* Note: when using "live keys": 
 * Y->key material, Y->SBoxkey
 */
.macro round_F out, in, tmp
;.print "round_F out, in, tmp"
local roll_start, roll_loop
    round_g_init
.if INLINE_round_g
    round_g tmp, in
    round_g tmp+4, in+7
.else
    clr r24  ; set the 'step' for the round_g function to zero
    shared round_g_rot, %tmp+4, %in
    xchgq tmp, tmp+4, 0
    clr r24
    shared round_g_rot, %tmp+4, %in
.endif
    pht tmp, tmp+4
    pop Z_L
    pop Z_H
    addldq tmp, Z+, r0, lpm
    addldq tmp+4, Z+, r0, lpm
    push Z_H
    push Z_L
    eorq out, tmp
    ror1q out
    clr r0
    rol1q out+4
    eorq out+4, tmp+4
.endm

/* swap the feistel data. Recommend not using this! */
.macro swap_halves a
;.print "swap_halves a"
.if UNROLL_swap
    ; unrolled: 12 instr, 12 cycles
    .irp i, 0,2,4,6
    movw r20, a+i
    movw a+i, a+8+i
    movw a+8+i, r20
    .endr
.else
local loop
    ; rolled: 8 instr, 89 cycles
    clr Z_H
    ldi Z_L, a
loop:
    ld r25, Z
    ldd r24, Z+8
    std Z+8, r25
    st Z+, r24
    cpi Z_L, (a+8)&0xFF
    brne loop
.endif
.endm

.macro whiten_tab ofs
.if UNROLL_whiten
    movw Z_L, Y_L
    .irp k, 4,8,12,16
    eorldq k, Z+, r0, lpm
    .endr
.else
    movw Z_L, Y_L
    ldi r20, 16
    la X, 4
1:  ld r1, X
    lpm r0, Z+
    eor r1, r0
    st X+, r1
    dec r20
    brne 1b
.endif
.endm

/*
 * r4..r19: data to encrypt
 * ---
 * r4..r19: encrypted block
 */

twofish_enc:

    ; pre-whitening
    la Y, twofish_roundkeys

    .if INLINE_whiten
    whiten_tab
    .else
    shared whiten_tab
    .endif
    movw Z_L, Y_L
    adiw Z_L, 32
    push Z_H
    push Z_L

    ; core rounds

.if UNROLL_enc
L_enc_loop:
    round_F 12, 4, 20
    round_F 4, 12, 20
.else
    rjmp 1f
L_enc_loop:
    swap_halves 4
1:  round_F 12, 4, 20
.endif
    sub Z_L, Y_L                ; deduce round from Y
    cpi Z_L, SCHEDULE_SIZE
    br lo, L_enc_loop

.if OMIT_last_swap == UNROLL_enc
    swap_halves 4
.endif

    ; post-whitening
    .if INLINE_whiten
    adiw Y_L, 16
    whiten_tab
    sbiw Y_L, 16
    .else
    adiw Y_L, 16
    shared whiten_tab
    sbiw Y_L, 16
    .endif
    pop Z_L
    pop Z_H
empty_function:
    ret
    .size twofish_enc, .-twofish_enc

.include "2fish_avr.key"
