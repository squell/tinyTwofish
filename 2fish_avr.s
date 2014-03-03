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

KEY_SIZE = 128
MDS_POLY = 0x169
RS_POLY  = 0x14D

/* we can share code between keysched. and encryption. should we? 
   note that this is incompatible with 'UNROLL_round_h' */
INLINE_round_g = 1

/* options controlling various size vs. speed tradeoffs */
UNROLL_round_h = 0
UNROLL_round_g = 1
UNROLL_keypair = 1
UNROLL_enc     = 1
UNROLL_swap    = 1

/* precompute the roundkeys */
TAB_key = 1

/* should we "undo" the last swap? this is pointless; has no effect unless UNROLL_enc */
UNDO_swap = 0

.global twofish_key, twofish_enc, twofish_reserve

.include "avrmacros.s"

.text
.subsection 1

FISH_START=.

.macro pht a, b
.print "pht a, b"
    addq a, b
    addq b, a
.endm

.macro gf_shl reg, poly
.print "gf_shl reg, poly"
local skip
    lsl reg
    brcc skip
    eor reg, poly
skip:
.endm

/* poly must be pre-shifted */
.macro gf_shr reg, poly
.print "gf_shr reg, poly"
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
.macro mds_column dst, src, poly, coef ; trying to 'roll' this barely gains anything
.print "mds_column dst, src, poly, coef"
    quad eor dst, src
    .irp val, 4,0
    gf_shr src, poly
    .irp i, 0,1,2,3
    sbrc coef, val+i
    eor dst+i, src
    .endr
    .endr
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
local loop, start, k128, k192, stride, ofs
.if UNROLL_round_h
    .ifc <step>, <r24>
    .error "Incompatible: INLINE_round_g=0 but UNROLL_round_h=1"
    .endif
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
    eorlddq dst, Y+0*step, r30
    qxlati dst, <1,0,1,0> 
.else
#? requires key to not straddle a 256-page boundary
   ; r24 and r25 'are still free' at this point, because of the order of calls to round_g in round_F
   ; they will always 'get overwritten later'. in case round_g is not inlined, the calls are carefully
   ; crafted to still ensure this.
.ifnc <dst>, <src>
    movq dst, src
.endif
.ifnc <step>, <r24>                  ; if r24, assume the caller takes responsibility
    ofs    = 0*step                  ; abusing the textual substitution here, again
    stride = step*0
    ldi r24, stride-4              
    adiw Y_L, stride*KEY_SIZE/64 + ofs
.else
    ofs = 0
.endif
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
.if ofs
    sbiw Y_L, ofs
.endif
.endif
.endif
.endif                      
.endm

.macro round_g_init
.print "round_g_init"
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
    ldi r31, hi8(qbox)
    round_h 0, src, step

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
.print "round_g_rot out, src"
    ; assume the caller set r24
    adiw Y_L, KEY_SIZE/16
    round_g out, src, r24
    xchgq src, src+7
.endm

.macro keypair kreg, num
.print "keypair kreg, num"
#? Y -> key material
local i, loop, exit, tmp            
.if UNROLL_keypair
    .if !INLINE_round_g
    .warning "Ignoring INLINE_round_g in .macro keypair, because of UNROLL_keypair"
    .endif
    .irp ofs, 0, 4
    quad mov 0, num                   ; OPT if num=0, save 1 
    inc num                           ; OPT not necessary the second time if TAB_key == 0
    round_g kreg+ofs, 0, <8+ofs>
    .endr
.else
    .if !TAB_key && !INLINE_round_g
    quad push 4                       ; !TAB_key is a annoying option
    .endif
loop:
    tmp = 4*!INLINE_round_g           ; use r4 as source, to mimick round_F
    movq kreg, kreg+4                 ; no-op on the first pass
    quad mov tmp, num  
    inc num
    .if INLINE_round_g
    round_g kreg+4, tmp, <8+0>
    .else
    ldi r24, 4       
    adiw Y_L, KEY_SIZE/16
    shared round_g_rot, %kreg+4, %tmp
	.if !TAB_key
	movq tmp+7, tmp               ; partly undo the last swap
	.endif
    .endif
    adiw Y_L, 4
    sbrc num, 0
    rjmp loop
    sbiw Y_L, 8
    .if !TAB_key && !INLINE_round_g
    quad pop 4,, <3,2,1,0>
    .endif
.endif
    ; we now have two intermediate results in kreg, kreg+4
    pht kreg, kreg+7
    clr r30
    rol1q kreg+4, r30
    ; note that the second key is still 'unrotated' by 16bits
.endm

/* In case TAB_key is 0, we need to compute roundkeys on the fly.
 * This happens in multiple places, so best to make a function.
 * Also this really causes register exhaustion. The only reason
 * to want this if you are low on SRAM.
 */

.macro keypair_wrap kreg, num
.print "keypair_wrap kreg, num"
    round_g_init
    adiw Y_L, KEY_SIZE/16 ; TODO OPT can go?
    keypair kreg, num
    sbiw Y_L, KEY_SIZE/16
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
    movw r14, X_L           ; save this position to return it to the caller as Y
    ldi r20, 40             ; all the 'good' registers could be occupied.
    mov r13, r20
    clr r12                 ; round number
    round_g_init
1:  keypair 16, r12         ; allocating on r16 means we can optimally share round_g (in some cases)
    .irp i, 0,1,2,3,6,7,4,5 ; fix the order of 2nd key
    st X+, (16+i)
    .endr
    cpse r12, r13           ; finally an excuse to use this instruction
    rjmp 1b
    movw Y_L, r14
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
 * Y->key material, Y->SBoxkey
 */
.macro round_F out, in, tmp
.print "round_F out, in, tmp"
local roll_start, roll_loop
.if !TAB_key
    shared keypair_wrap, 20, r16
    ; swap r16 with top of stack
    mov r30, r16
    pop r16
    push r30
    .irp i, 5,4,7,6,3,2,1,0
    push tmp+i
    .endr
.endif
    round_g_init
.if INLINE_round_g
    round_g tmp, in
    round_g tmp+4, in+7
.else
    clr r24  ; set the 'step' for the round_g function to zero
    shared round_g_rot, %tmp+4, %in
    xchgq tmp, tmp+4
    clr r24
    shared round_g_rot, %tmp+4, %in
.endif
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
.if !TAB_key
    ; switch back
    mov r0, r16
    pop r16
    push r0
.endif
.endm

/* swap the feistel data. Recommend not using this! */
.macro swap_halves a
.print "swap_halves a"
.if !TAB_key                      ; OPT integrate below
    mov r25, r16                  ; this is necessary since we steal r16
    pop r16
    push r8
    mov r8, r25
.endif
.if UNROLL_swap
    .irp i, 0,1,2,3,4,5,6,7
    xchg i+a, i+a+8, r25
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

.if !UNROLL_enc && UNDO_swap
    .warning "Ignoring UNDO_swap since UNROLL_enc = 0"
    UNDO_swap=0
.endif

/*
 * Y -> pointer to start of (round)key/end of RS-key
 * r4..r19: data to encrypt
 * ---
 * r4..r19: encrypted block
 */

; TODO: make two macro's to seperate the whitening stages from the main loop to make the code more readable
twofish_enc:

    ; pre-whitening

.if TAB_key
    movw Z_L, Y_L
    .irp k, 4,8,12,16
    eorldq k, Z+
    .endr
    adiw Z_L, 16
    push Z_H
    push Z_L
.endif
    sbiw Y_L, KEY_SIZE/16       ; TODO OPT can be avoided?
.if !TAB_key
    .irp j, 4,12                ; OPT we could save a few bytes by rolling this loop
    push r16
    ldi r16, (j-4)/4            ; round counter: overlaps with data; not ideal
    shared keypair_wrap, 20, r16
    pop r16
    zip eor j,   20
    zip eor j+6, 24             ; remember: odd keys are unrotated.
    .endr
    push r16
    ldi r16, 8
.endif

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
.if TAB_key
    sub Z_L, Y_L
    cpi Z_L, twofish_reserve
.else
    cpi r16, 40
.endif
    loop lo, L_enc_loop

.if UNDO_swap
    swap_halves 4
.endif

    ; post-whitening

.if TAB_key
    adiw Y_L, 32                ; TODO OPT this can be avoided?
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
.else
    .irp j, 4, 12 
    .if UNDO_swap
    ldi r16, 3+j/4
    .else        
    ldi r16, 7-j/4
    .endif
    shared keypair_wrap, 20, r16
    .if j==12
    pop r16
    .endif
    eorq j, 20
    eorq j+6, 24
    .endr
.endif
    ret
    .size twofish_enc, .-twofish_enc

.macro dump load org
.print "dump load org"
local i
    la Z, org
    i=0
    .rept 30
    load i, Z+
    i=i+1
    .endr
    ser r31
.endm

.macro qcompute reg, tmp=r0
    mov tmp, reg    ; reg: a|b
    andi tmp, 0xF   
    swap tmp        ; tmp: b|_
    xor reg, tmp
    eor tmp, <a|_>  ; 
    lsr reg         
    andi reg, 0xF   ; reg: _|ror(b,4)^8a&0x10

.endm

.text 2048
FISH_END = .-FISH_START

.text

main:
    la Y, .data
    la Z, mkey
    copy Y, KEY_SIZE/8
    la X, .data+32
    rcall twofish_key
    .if !TAB_key
    movw X_L, Y_L
    la Z, mkey
    copy X, KEY_SIZE/8
    .endif

    la Z, 4
    clr r0
1:  st Z+, r0
    cpi Z_L, 20
    brlo 1b

    rcall twofish_enc

    cli
    sleep
    .size main, .-main

.subsection 4096
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
zero:
.space 32
skey:
.int 0, 0
rkey:
.int 0x52C54DDE,0x11F0626D,0x7CAC9D4A,0x4D1B4AAA,0xB7B83A10,0x1E7D0BEB,0xEE9C341F,0xCFE14BE4,0xF98FFEF9,0x9C5B3C17,0x15A48310,0x342A4D81,0x424D89FE,0xC14724A7,0x311B834C,0xFDE87320,0x3302778F,0x26CD67B4,0x7A6C6362,0xC2BAF60E,0x3411B994,0xD972C87F,0x84ADB1EA,0xA7DEE434,0x54D2960F,0xA2F7CAA8,0xA6B8FF8C,0x8014C425,0x6A748D1C,0xEDBAF720,0x928EF78C,0x0338EE13,0x9949D6BE,0xC8314176,0x07C07D68,0xECAE7EA7,0x1FE71844,0x85C05C89,0xF298311E,0x696EA672

.data
blaat: .int 0x666
