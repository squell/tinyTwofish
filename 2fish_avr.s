/*
  tinyTwofish implementation for tinyAVR (ATtiny25/45/85)

  Copyright (C) 2001, 2014 Marc Schoolderman

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

; TODO FIXME WORK IN PROGRESS

KEY_SIZE = 256
MDS_POLY = 0x169
RS_POLY  = 0x14D

/* options controlling various size vs. speed tradeoffs */
UNROLL_round_h = 0
UNROLL_round_g = 0
UNROLL_keypair = 0
UNROLL_enc     = 0
UNROLL_swap    = 0

/* precompute the roundkeys */
TAB_key = 1

/* should we "undo" the last swap? this is pointless; has no effect unless UNROLL_enc */
UNDO_swap = 1

.global twofish_key, twofish_enc

.include "avrmacros.s"

.text

    ldi r31, pm_hi8(main)
    ldi r30, pm_lo8(main)
    ijmp 

.macro pht a, b
    addq a, b
    addq b, a
.endm

.macro gf_shl reg, poly
local skip
    lsl reg
    brcc skip
    eor reg, poly
skip:
.endm

/* poly must be pre-shifted */
.macro gf_shr reg, poly
local skip
    lsr reg
    brcc skip
    eor reg, poly
skip:
.endm

/* tables should be aligned on 256-byte boundary 
   this macro can perform the rotation on the fly */
.macro sxlat r, rotate=0, load=ld
#?  ldi r31, hi8(table) 
#?  ldi r29, hi8(table) 
    mov r30, r+(4-rotate/8)%4
    mov r28, r+(1-rotate/8)%4 
    load r+0, Z
    mov r30, r+(2-rotate/8)%4
    load r+1, Y
    mov r28, r+(3-rotate/8)%4
    load r+2, Z
    load r+3, Y
.endm

/* perform qbox lookup & (optional) copy */
.macro qxlati d, select, r=n/a, load=lpm
#?  ldi r31, hi8(table)
local i
.irp j, 0,1
i=0
.irp k, select
    .if k==j
    .ifc <r>, <n/a>
    mov r30, d+i
    .else
    mov r30, (r&&~3)+(r+i)%4
    .endif
    load d+i, Z
    .endif
    i=i+1
.endr
    subi r31, 2*j-1
.endr
.endm

# select is now a register from which the lower nibble controls the lookup
# expects table to be 512-byte aligned
.macro qxlat d, select, wiring=<0,1,2,3>, r=, load=lpm
#?  ldi r31, hi8(table)
local i
i=0
.irp j, wiring
    bst select, j
    bld r31, 0
    .ifc <r>, <n/a>
    mov r30, d+i
    .else
    mov r30, r+i
    .endif
    load d+i, Z
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
.macro mds_column dst, src, poly, coef
    quad eor dst, src
.irp val, 4,0
    gf_shr src, poly
.irp i, 0,1,2,3
    sbrc coef, val+i
    eor dst+i, src
.endr
.endr
.endm

/* sacrifice all speed for a few bytes; currently BROKEN TODO */
.macro mds_column_slow dst, src, poly, coef
local not_EF, not_5B, loop
    ldi r28, dst
    clr r29
    ldi r30, 4
loop:
    ld r2, Y
    eor r2, src
    mov r1, src
    gf_shr r1, poly
    lsl coef
    brcc not_EF
    eor r2, r1
not_EF:
    brhc not_5B
    gf_shr r1, poly
    eor r2, r1
not_5B:
    st Y+, r2
    dec r30
    brne loop
.endm

/* maintain the RS-state in r0..r3 */
#  g(x) = x**4 + (a + 1/a) x**3 + a x**2 + (a + 1/a) x + 1
.macro reedsolomon K, load=ld, tmp=4
local loop
    quad clr 0
    ldi r16, 8      ; clobbers r16..18
    ldi r17, RS_POLY>>1
    ldi r18, RS_POLY&&0xFF
loop:
    movw tmp, r0    ; save r0,1
    load r0, K      ; get top raw byte
    eor r0, r3      ; [0] 1
    mov r1, r0
    mov r3, r0
    gf_shl r1, r18  ; r1 = (a)x
    gf_shr r3, r17
    eor r3, r1      ; r3 = (a+1/a)x
    eor tmp+1, r1   ;
    mov r1, tmp
    eor r1, r3      ; [1] (a+1/a)
    eor r3, r2      ; [3] (a+1/a)
    mov r2, tmp+1   ; [2] (a)
    dec r16
    brne loop
.endm

/* you are not required to understand this */
.equ twofish_cookie, (0b10100110<<((288-KEY_SIZE)/42))&0xFF

.equ twofish_reserve, KEY_SIZE/16 + 40*4*TAB_key

.macro round_h dst, src, step=<8+0>
#? Y -> key material
local loop, start, k128, k192
.if UNROLL_round_h
.if KEY_SIZE > 192
    qxlati dst, <1,0,0,1>, src
    eorlddq dst, Y+3*step, r30      ; note: step is textually substituted
    qxlati dst, <1,1,0,0>           ; (this is hacky, but it works)
    eorlddq dst, Y+2*step, r30
    qxlati dst, <0,1,0,1>
.elseif KEY_SIZE > 128
    qxlati dst, <1,1,0,0>, src
    eorlddq dst, Y+2*step, r30
    qxlati dst, <0,1,0,1>
.else
    qxlati dst, <0,1,0,1>, src
.endif
    eorlddq dst, Y+1*step, r30
    qxlati dst, <0,0,1,1> 
    eorlddq dst, Y+0*step, r30       ; again: textual substitution! 
    qxlati dst, <1,0,1,0> 
.else
#? requires key to not straddle a 256-page
    ; step now a register: contains 24 if 8 byte strides, 25 if offset, 16 if 4-byte strides
.ifnc <dst>, <src>
    movq dst, src
.endif
    ldi r24, step*0-4                ; abuse of textual substitution
    adiw Y_L, (step*0)*KEY_SIZE/64 + 0*step ; again
    ldi r25, twofish_cookie
    rjmp start
loop:
    sub Y_L, r24
    eorldq dst, -Y, r30
start:
    qxlat dst, r25, <7,4,6,5>
    lsl r25                          ; magic
    brcc loop           
.if KEY_SIZE > 128
    brhs loop
.if KEY_SIZE > 192
    lsl r25
    brne loop
.if 0*step
    sbiw Y_L, 0*step   
.endif
.endif
.endif
.endif                      
.endm

.macro round_g_init
.if UNROLL_round_g
    ldi r31, hi8(qbox)
.endif
.endm

/* uses r0..r3 as working area */
.macro round_g out, src, step=<4+0>
#? Y -> key material
local i, loop
.if UNROLL_round_g
    round_h 0, src, step
    ldi r30, MDS_POLY>>1
    mds_columni out, r0, r30, <0x01,0x5B,0xEF,0xEF>, mov
    mds_columni out, r1, r30, <0xEF,0xEF,0x5B,0x01>
    mds_columni out, r2, r30, <0x5B,0xEF,0x01,0xEF>
    mds_columni out, r3, r30, <0x5B,0x01,0xEF,0x5B>
.else
    push Y_L ;( we are out of registers.
    ; an encoding of the MDS matrix
    .irp controlword, 0b01001101, 0b10101011, 0b00110111, 0b11001110
    ldi r30, controlword
    push r30
    .endr
    ldi r31, hi8(qbox)
    round_h 0, src, step
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

.macro keypair kreg, num
#? Y -> key material
local i, loop, exit
.if UNROLL_keypair
    .irp ofs, 0, 4
    quad mov 0, num
    inc num            ; TODO not necessary the second time if TAB_key == 0
    round_g kreg+ofs, 0, <8+ofs>
    .endr
.else
loop:
    movq kreg, kreg+4  ; no-op on the first pass
    quad mov 0, num
    inc num
    round_g kreg+4, 0, <8+0>
    adiw Y_L, 4
    sbrc num, 0
    rjmp loop
    sbiw Y_L, 8
.endif
    ; we now have two intermediate results in kreg, kreg+4
    pht kreg, kreg+7
    clr r30
    rol1q kreg+4, r30
    ; note that the second key is still 'unrotated' by 16bits
.endm

/*
 * Y -> pointer to *end* of master key
 *      (really, this makes a lot of sense)
 * X -> holding area for key material
 * ---
 * Y -> *end* of RS-key, start of roundkeys
 *
 * All keys are stored in reverse order.
 */

twofish_key:
    ldi r20, KEY_SIZE/64
1:  reedsolomon -Y
    .irp i, 0,1,2,3         ; order: stores the last sbox key first
    st X+, i
    .endr
    dec r20
    brne 1b
.if TAB_key
    movw r20, X_L
    ldi r17, 0              ; round number
    round_g_init
1:  keypair 4, r17
    .irp i, 0,1,2,3,6,7,4,5 ; fix the order of 2nd key
    st X+, (4+i)
    .endr
    cpi r17, 40
    loop ne, 1b
    movw Y_L, r20
.else
    movw Y_L, X_L
.endif
1:  ret
    .size twofish_key, .-twofish_key

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
 * [stack]->key material, Y->SBoxkey
 */
.macro round_F out, in, tmp
.if !TAB_key
    keypair tmp, num
    quad push tmp,, <5,4,7,6,3,2,1,0>
.endif
    la Z, qbox
    round_g_init
    round_g tmp, in
    round_g tmp+4, in+7
    pht tmp, tmp+4
.if TAB_key
    pop Z_L
    pop Z_H
    addldq tmp, Z+
    addldq tmp+4, Z+
    push Z_H
    push Z_L
.else
    .irp j, 0, 4
    pop r30
    add tmp+j, r30
    .irp i, 1,2,3
    pop r30
    adc tmp+j+i, r30
    .endr
    .endr
.endif
    eorq out, tmp
    ror1q out
    clr r0
    rol1q out+4
    eorq out+4, tmp+4
.endm

/* swap the feistal data. Recommend not using this! */
.macro swap_halves a
.if UNROLL_swap
    .irp i, 0,1,2,3,4,5,6,7
    mov r25, i+a
    mov i+a, i+a+8
    mov i+a+8, r25
    .endr
.else
    ; rolled: 8 instr, 89 cycles
    clr Z_H
    ldi Z_L, a
1:  ld r24, Z
    ldd r25, Z+8
    std Z+8, r24
    st Z+, r25
    cpi Z_L, a+8
    brlo 1b
.endif
.endm

/*
 * Y -> pointer to start of roundkey/end of RS-key
 * r4..r19: data to encrypt
 * ---
 * r4..r19: encrypted block
 */

twofish_enc:
.if TAB_key
    movw Z_L, Y_L
    .irp k, 4,8,12,16
    eorldq k, Z+
    .endr
    adiw Z_L, 16
    push Z_H
    push Z_L
.endif
    sbiw Y_L, KEY_SIZE/16        ; TODO optimise this away

.if UNROLL_enc
L_enc_loop:
    round_F 12, 4, 20
    round_F 4, 12, 20
.else
L_enc_loop:
1:  round_F 12, 4, 20
    push Z_L
    swap_halves 4
    pop Z_L
.endif
    sub Z_L, Y_L
    cpi Z_L, 40*4+1        ; would like to have 'brle'
    loop lo, L_enc_loop

    adiw Y_L, 32
.if UNROLL_enc && UNDO_swap
    swap_halves 4
.endif
.if UNROLL_enc && !UNDO_swap
    .irp k, 12,16,4,8
    eorldq k, Y+
    .endr
.else
    .irp k, 4,8,12,16
    eorldq k, Y+
    .endr
.endif
    pop Z_L
    pop Z_H
    ret
    .size twofish_enc, .-twofish_enc

.macro dump load org
local i
    la Z, org
    i=0
    .rept 30
    load i, Z+
    i=i+1
    .endr
    ser r31
.endm

main:
    cli 
    la Z, mkey
    la X, .data+32
    la Y, .data
    copy Y, KEY_SIZE/8
    rcall twofish_key
    .irp j, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
    clr 4+j
    .endr
    la Y, .data+32+16
    rcall twofish_enc

    sleep
    .size main, .-main

CODE_END = .

.balign 512
qbox:
    .byte 0xa9, 0x67, 0xb3, 0xe8, 0x04, 0xfd, 0xa3, 0x76, 0x9a, 0x92, 0x80, 0x78, 0xe4, 0xdd, 0xd1, 0x38 
    .byte 0x0d, 0xc6, 0x35, 0x98, 0x18, 0xf7, 0xec, 0x6c, 0x43, 0x75, 0x37, 0x26, 0xfa, 0x13, 0x94, 0x48 
    .byte 0xf2, 0xd0, 0x8b, 0x30, 0x84, 0x54, 0xdf, 0x23, 0x19, 0x5b, 0x3d, 0x59, 0xf3, 0xae, 0xa2, 0x82 
    .byte 0x63, 0x01, 0x83, 0x2e, 0xd9, 0x51, 0x9b, 0x7c, 0xa6, 0xeb, 0xa5, 0xbe, 0x16, 0x0c, 0xe3, 0x61 
    .byte 0xc0, 0x8c, 0x3a, 0xf5, 0x73, 0x2c, 0x25, 0x0b, 0xbb, 0x4e, 0x89, 0x6b, 0x53, 0x6a, 0xb4, 0xf1 
    .byte 0xe1, 0xe6, 0xbd, 0x45, 0xe2, 0xf4, 0xb6, 0x66, 0xcc, 0x95, 0x03, 0x56, 0xd4, 0x1c, 0x1e, 0xd7 
    .byte 0xfb, 0xc3, 0x8e, 0xb5, 0xe9, 0xcf, 0xbf, 0xba, 0xea, 0x77, 0x39, 0xaf, 0x33, 0xc9, 0x62, 0x71 
    .byte 0x81, 0x79, 0x09, 0xad, 0x24, 0xcd, 0xf9, 0xd8, 0xe5, 0xc5, 0xb9, 0x4d, 0x44, 0x08, 0x86, 0xe7 
    .byte 0xa1, 0x1d, 0xaa, 0xed, 0x06, 0x70, 0xb2, 0xd2, 0x41, 0x7b, 0xa0, 0x11, 0x31, 0xc2, 0x27, 0x90 
    .byte 0x20, 0xf6, 0x60, 0xff, 0x96, 0x5c, 0xb1, 0xab, 0x9e, 0x9c, 0x52, 0x1b, 0x5f, 0x93, 0x0a, 0xef 
    .byte 0x91, 0x85, 0x49, 0xee, 0x2d, 0x4f, 0x8f, 0x3b, 0x47, 0x87, 0x6d, 0x46, 0xd6, 0x3e, 0x69, 0x64 
    .byte 0x2a, 0xce, 0xcb, 0x2f, 0xfc, 0x97, 0x05, 0x7a, 0xac, 0x7f, 0xd5, 0x1a, 0x4b, 0x0e, 0xa7, 0x5a 
    .byte 0x28, 0x14, 0x3f, 0x29, 0x88, 0x3c, 0x4c, 0x02, 0xb8, 0xda, 0xb0, 0x17, 0x55, 0x1f, 0x8a, 0x7d 
    .byte 0x57, 0xc7, 0x8d, 0x74, 0xb7, 0xc4, 0x9f, 0x72, 0x7e, 0x15, 0x22, 0x12, 0x58, 0x07, 0x99, 0x34 
    .byte 0x6e, 0x50, 0xde, 0x68, 0x65, 0xbc, 0xdb, 0xf8, 0xc8, 0xa8, 0x2b, 0x40, 0xdc, 0xfe, 0x32, 0xa4 
    .byte 0xca, 0x10, 0x21, 0xf0, 0xd3, 0x5d, 0x0f, 0x00, 0x6f, 0x9d, 0x36, 0x42, 0x4a, 0x5e, 0xc1, 0xe0

    .byte 0x75, 0xf3, 0xc6, 0xf4, 0xdb, 0x7b, 0xfb, 0xc8, 0x4a, 0xd3, 0xe6, 0x6b, 0x45, 0x7d, 0xe8, 0x4b 
    .byte 0xd6, 0x32, 0xd8, 0xfd, 0x37, 0x71, 0xf1, 0xe1, 0x30, 0x0f, 0xf8, 0x1b, 0x87, 0xfa, 0x06, 0x3f 
    .byte 0x5e, 0xba, 0xae, 0x5b, 0x8a, 0x00, 0xbc, 0x9d, 0x6d, 0xc1, 0xb1, 0x0e, 0x80, 0x5d, 0xd2, 0xd5 
    .byte 0xa0, 0x84, 0x07, 0x14, 0xb5, 0x90, 0x2c, 0xa3, 0xb2, 0x73, 0x4c, 0x54, 0x92, 0x74, 0x36, 0x51 
    .byte 0x38, 0xb0, 0xbd, 0x5a, 0xfc, 0x60, 0x62, 0x96, 0x6c, 0x42, 0xf7, 0x10, 0x7c, 0x28, 0x27, 0x8c 
    .byte 0x13, 0x95, 0x9c, 0xc7, 0x24, 0x46, 0x3b, 0x70, 0xca, 0xe3, 0x85, 0xcb, 0x11, 0xd0, 0x93, 0xb8 
    .byte 0xa6, 0x83, 0x20, 0xff, 0x9f, 0x77, 0xc3, 0xcc, 0x03, 0x6f, 0x08, 0xbf, 0x40, 0xe7, 0x2b, 0xe2 
    .byte 0x79, 0x0c, 0xaa, 0x82, 0x41, 0x3a, 0xea, 0xb9, 0xe4, 0x9a, 0xa4, 0x97, 0x7e, 0xda, 0x7a, 0x17 
    .byte 0x66, 0x94, 0xa1, 0x1d, 0x3d, 0xf0, 0xde, 0xb3, 0x0b, 0x72, 0xa7, 0x1c, 0xef, 0xd1, 0x53, 0x3e 
    .byte 0x8f, 0x33, 0x26, 0x5f, 0xec, 0x76, 0x2a, 0x49, 0x81, 0x88, 0xee, 0x21, 0xc4, 0x1a, 0xeb, 0xd9 
    .byte 0xc5, 0x39, 0x99, 0xcd, 0xad, 0x31, 0x8b, 0x01, 0x18, 0x23, 0xdd, 0x1f, 0x4e, 0x2d, 0xf9, 0x48 
    .byte 0x4f, 0xf2, 0x65, 0x8e, 0x78, 0x5c, 0x58, 0x19, 0x8d, 0xe5, 0x98, 0x57, 0x67, 0x7f, 0x05, 0x64 
    .byte 0xaf, 0x63, 0xb6, 0xfe, 0xf5, 0xb7, 0x3c, 0xa5, 0xce, 0xe9, 0x68, 0x44, 0xe0, 0x4d, 0x43, 0x69 
    .byte 0x29, 0x2e, 0xac, 0x15, 0x59, 0xa8, 0x0a, 0x9e, 0x6e, 0x47, 0xdf, 0x34, 0x35, 0x6a, 0xcf, 0xdc 
    .byte 0x22, 0xc9, 0xc0, 0x9b, 0x89, 0xd4, 0xed, 0xab, 0x12, 0xa2, 0x0d, 0x52, 0xbb, 0x02, 0x2f, 0xa9 
    .byte 0xd7, 0x61, 0x1e, 0xb4, 0x50, 0x04, 0xf6, 0xc2, 0x16, 0x25, 0x86, 0x56, 0x55, 0x09, 0xbe, 0x91

PROGRAM_END = .

mkey:
.byte 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef
.byte 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10
.byte 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77
.byte 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff
zero:
.space 32
skey:
.int 0, 0
rkey:
.int 0x52C54DDE,0x11F0626D,0x7CAC9D4A,0x4D1B4AAA,0xB7B83A10,0x1E7D0BEB,0xEE9C341F,0xCFE14BE4,0xF98FFEF9,0x9C5B3C17,0x15A48310,0x342A4D81,0x424D89FE,0xC14724A7,0x311B834C,0xFDE87320,0x3302778F,0x26CD67B4,0x7A6C6362,0xC2BAF60E,0x3411B994,0xD972C87F,0x84ADB1EA,0xA7DEE434,0x54D2960F,0xA2F7CAA8,0xA6B8FF8C,0x8014C425,0x6A748D1C,0xEDBAF720,0x928EF78C,0x0338EE13,0x9949D6BE,0xC8314176,0x07C07D68,0xECAE7EA7,0x1FE71844,0x85C05C89,0xF298311E,0x696EA672

