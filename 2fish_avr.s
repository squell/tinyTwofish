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

UNROLL_round_h = 0
UNROLL_round_g = 0
INLINE_mds = 1 ; TODO
TAB_key = 1

X_L  = 26
X_H  = 27
Y_L  = 28
Y_H  = 29
Z_L  = 30
Z_H  = 31
SP_H = 0x3e
SP_L = 0x3d
SREG = 0x3f

.global twofish_key, twofish_enc

.text

    ldi r31, pm_hi8(main)
    ldi r30, pm_lo8(main)
    ijmp 

/* note: require all 32bit values to be 4-byte aligned, we can use a 
   wrap-around trick to get 8bit-rotations for free */

.macro quad op reg, arg, range=<0,1,2,3>
.irp i, range
.ifnc <>, <arg>
    op (reg&&~3)+(reg+i)%4, arg
.else
    op (reg&&~3)+(reg+i)%4
.endif
.endr
.endm

.macro zip op rd, rr, range=<0,1,2,3>
.irp i, range
    op (rd&&~3)+(rd+i)%4, (rr&&~3)+(rr+i)%4
.endr
.endm

.macro la M, ofs
    ldi M&_L, lo8(ofs)
    ldi M&_H, hi8(ofs)
.endm

.macro far call fun
    ldi Z_L, pm_lo8(ofs)
    ldi Z_H, pm_hi8(ofs)
    call
.endm

.macro addq a, b
    add a, b
    zip adc a, b, <1,2,3>
.endm

.macro eorq a, b
    zip eor a, b
.endm

.macro movq a, b
.if (a%2)==0 && (b%2)==0
    movw a, b
    movw (a&&~3)+(a+2)%4, (b&&~3)+(b+2)%4
.else 
    ; we could save an instruction here by case analysis
    zip mov a, b 
.endif
.endm

.macro eorlddq a, M, tmp=r30
.irp i, 0,1,2,3
    ldd tmp, M+i
    eor a+i, tmp
.endr
.endm

/* do some macro hacking to find the directionality */
.macro eorldq a, M, tmp=r30
.irpc c, M
.ifc <c>, <+>
    .irp i, 0,1,2,3
	ld tmp, M
	eor a+i, tmp
    .endr
    .exitm
.endif
.ifc <c>, <->
    .irp i, 3,2,1,0
	ld tmp, M
	eor a+i, tmp
    .endr
    .exitm
.endif
.endr
.endm

.macro addldq a, M, tmp=r30, load=ld
    load tmp, M
    add a, tmp
.irp i, 1,2,3
    load tmp, M
    adc a+i, tmp
.endr
.endm

.macro rol1q a, null=r0
    addq a, a
    adc a, null
.endm

.macro ror1q a, tmp=r30
    mov tmp, a
    lsr tmp
.irp i, 3,2,1,0
    ror a+i
.endr
.endm

.macro pht a, b, rot=0
    addq a, b+rot
    addq b+rot, a
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
    mov r30, r+i
    .endif
    load d+i, Z
    .endif
    .set i, i+1
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
    .set i, i+1
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
    .set i, i+1
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

.macro round_h dst, src, step=8+0
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
    adiw Y_L, KEY_SIZE/8 + 0*step    ; abuse of textual substitution
    ldi r25, twofish_cookie
    rjmp start
loop:
    sub Y_L, r24
    eorldq dst, -Y, r30
start:
    qxlat dst, r25, <7,4,6,5>
    lsl r25             ; magic
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

.macro round_g_init poly
.if UNROLL_round_g
    ldi r31, hi8(qbox)
.else
    ; an encoding of the MDS matrix
    ; TODO FIXME analyse available registers
    ldi r16, 0b11001110
    ldi r17, 0b00110111
    ldi r18, 0b10101011
    ldi r19, 0b01001101
.endif
    ldi poly, MDS_POLY>>1
.endm

/* uses r0..r3 as working area */
.macro round_g out, src, poly, ofs=0
#? Y -> key material
local i, loop
    round_h 0, src, <8+ofs>
.if UNROLL_round_g
    mds_columni out, r0, poly, <0x01,0x5B,0xEF,0xEF>, mov
    mds_columni out, r1, poly, <0xEF,0xEF,0x5B,0x01>
    mds_columni out, r2, poly, <0x5B,0xEF,0x01,0xEF>
    mds_columni out, r3, poly, <0x5B,0x01,0xEF,0x5B>
.else
    quad clr out
    clr r31      ; use Z to access the register file
    clr r30      
loop:
    ldd r25, Z+16
    ld r0, Z+
    mds_column out, r0, poly, r30
    cpi r30, 4
    brlo loop
.endif
.endm

.macro keypair kreg, num, poly
#? Y -> key material
local i, loop, exit
.if UNROLL_round_g
    .irp ofs, 0, 4
    quad mov 0, num
    inc num
    round_g kreg+ofs, 0, poly, ofs
    .endr
.else
loop:
    movq kreg, kreg+4  ; no-op on the first pass
    quad mov 0, num
    inc num
    round_g kreg+4, 0, poly
    adiw Y_L, 4
    sbrc num, 0
    rjmp loop
    sbiw Y_L, 8
.endif
    ; we now have two intermediate results in kreg, kreg+4
    pht kreg, kreg+4, 3
    clr r30
    rol1q kreg+4, r30
    ; note that the second key is still 'unrotated' by 16bits
.endm

/*
register allocation plan:
 r0.. r3: work area (for MDS in particular)
 r4.. r7,r8..r11: feistel half
 r12..       r19: other half
 r20..       r28: data

round-keys: add in from memory; there is not enough room, and saving
one of the feistel half isn't any better. 

 */

.macro round_F num, out, in, tmp, poly
.ifne TAB_key
    keypair tmp, num, poly
    quad push tmp,, <5,4,7,6,3,2,1,0>
.endif
    round_g tmp,   in,   poly
    round_g tmp+4, in+7, poly
    pht tmp, tmp+4
.if TAB_key
    addldq tmp, Y+
    addldq tmp+4, Y+
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
.endm

/*
 * Y -> pointer to *end* of master key
 *      (really, this makes a lot of sense)
 * X -> holding area for key material
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
    clr r17                 ; round number
    round_g_init r16
1:  keypair 4, r17, r16
    .irp i, 0,1,2,3,6,7,4,5 ; fix the order of 2nd key
    st X+, (4+i)
    .endr
    cpi r17, 40
    brsh 2f                 ; TODO: relative jump when?
    rjmp 1b
2:  
.endif
    ret

.macro copy D, size, cntr=n/a, tmp=r0
.ifc <n/a>, <cntr>
    .rept size
    lpm tmp, Z+
    st D&+, tmp
    .endr
.else
local cp
    ldi cntr, size
cp: lpm tmp, Z+
    st D&+, tmp
    dec cntr
    brne cp
.endif
.endm

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
    la Z, mkey
    la X, .data+32
    la Y, .data
    copy Y, KEY_SIZE/8
    rcall twofish_key
    dump ld .data+32+16
    cli
    sleep
    .size main, .-.text

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

mkey:
.byte 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef
.byte 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10
.byte 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77
.byte 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff
skey:
.int 0, 0
rkey:
.int 0x52C54DDE,0x11F0626D,0x7CAC9D4A,0x4D1B4AAA,0xB7B83A10,0x1E7D0BEB,0xEE9C341F,0xCFE14BE4,0xF98FFEF9,0x9C5B3C17,0x15A48310,0x342A4D81,0x424D89FE,0xC14724A7,0x311B834C,0xFDE87320,0x3302778F,0x26CD67B4,0x7A6C6362,0xC2BAF60E,0x3411B994,0xD972C87F,0x84ADB1EA,0xA7DEE434,0x54D2960F,0xA2F7CAA8,0xA6B8FF8C,0x8014C425,0x6A748D1C,0xEDBAF720,0x928EF78C,0x0338EE13,0x9949D6BE,0xC8314176,0x07C07D68,0xECAE7EA7,0x1FE71844,0x85C05C89,0xF298311E,0x696EA672

PROGRAM_END = .
