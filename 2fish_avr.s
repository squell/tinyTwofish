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

.include "2fish_avr.cfg"
.include "avrmacros.s"

.global twofish_init, twofish_key, twofish_enc

.text 512

FISH_START = .

.macro pht a, b
;.print "pht a, b"
    addq a, b
    addq b, a
.endm

MDS_POLY = 0x169
RS_POLY  = 0x14D

.macro gf_shl reg, poly
;.print "gf_shl reg, poly"
local skip
    lsl reg
    brcc skip
    eor reg, poly
skip:
.endm

/* poly must be pre-shifted */
.macro gf_shr reg, poly
;.print "gf_shr reg, poly"
local skip
    lsr reg
    brcc skip
    eor reg, poly
skip:
.endm

/* perform the qbox lookup on the fly if needed */
.macro qstep dst, tmp, ofs, load=lpm
#? Z -> qbox
    mov tmp, r30    ; a|b
    swap tmp        ; b|a
    andi r30, 0xF   ; _|b 
    eor r30, tmp    ; b|b^a
    mov dst, r30 
    swap dst        ; b^a|b
    lsr dst         ; ?|b>>>1 ^ 8a
    eor tmp, dst    ; ?|b>>>1 ^ 8a ^ a

    andi r30, 0xF 
    or r30, ofs
    load r30, Z     ; t0[]
    andi r30, 0xF0
    mov dst, r30    ; necessary, since dst may be < r16

    mov r30, tmp
    andi r30, 0xF 
    or r30, ofs
    load tmp, Z     ; t1[]
    andi tmp, 0x0F
    or dst, tmp
.endm

.macro qbox_m dst, tmp, ofs
;.print "qbox_m dst, tmp, ofs"
#? r30 = value; T flag selects q-box
local loop
    ldi ofs, lo8(qperm)
    bld ofs, 5
loop:
    qstep dst, tmp, ofs
    subi ofs, 0xF0
    mov r30, dst
    sbrc ofs, 4      ; falls through on the second pass
    rjmp loop
    swap dst
.endm

/* perform qbox lookup & (optional) copy */
.macro qxlati d, select, r=n/a
.if !TAB_q
.error "Incompatible: TAB_q=0 but UNROLL_round_h=1"
.endif
#?  ldi r31, hi8(table)
local i
.irp j, 0,1
i=0
.irp k, select
    .if k==j
    .ifc <r>, <n/a>
    mov r30, d+i
    .else
    mov r30, ((r)&~3)+((r)+i)%4
    .endif
    load d+i, Z, SRAM_q
    .endif
    i=i+1
.endr
    subi r31, 2*j-1
.endr
.endm

/* perform sbox lookup & (optional) copy - modified from above */
.macro sxlati d, r=n/a
local i
i=0
.irp j, 0,0,0,2
    .ifc <r>, <n/a>
    mov r30, d+i
    .else
    mov r30, ((r)&~3)+((r)+i)%4
    .endif
    ld d+i, Z
    subi r31, 2*j-1
    i=i+1
.endr
.endm

# select is now a register from which the lower nibble controls the lookup
# expects table to be 512-byte aligned
.macro qxlat d, select, wiring=<0,1,2,3>, r=
#?  ldi r31, hi8(table)
local i
i=0
.irp j, wiring
    bst select, j
    .ifc <r>, <n/a>
    mov r30, d+i
    .else
    mov r30, r+i
    .endif
    .if TAB_q
    bld r31, 0
    load d+i, Z, SRAM_q
    .else
    ldi Z_H, hi8(qperm)
    qbox_m %d+i, %select+2, %select+1
    .endif
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
.equ twofish_cookie, (0b10100110<<((300-KEY_SIZE)/50))&0xFF

.macro round_h dst, src, step=<8+0>, tmpw=
;.print "round_h dst, src, step, tmpw"
#? Y -> key material
local loop, start, k128, k192, stride, ofs
.if TAB_sbox == 1
    sxlati dst, src                 ; round_h simply a lookup in this case
.elseif UNROLL_round_h
    .ifc <step>, <tmpw>
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
    .ifnc <dst>, <src>
    movq dst, src
    .endif
    .ifnc <step>, <tmpw>             ; if r24, assume the caller has set the skip-distance in r24
    ofs    = 0*step                  ; and modified Y_L
    stride = step*0
    ldi tmpw, stride-4              
    adiw Y_L, stride*KEY_SIZE/64 + ofs
    .else
    adiw Y_L, KEY_SIZE/16
    ofs = 0
    .endif
    ldi tmpw+1, twofish_cookie       ; load the magic cookie
    rjmp start
loop:
    clr r30                          ; we can save 2 instrs if we want, if we require Y to not 'wrap'
    sub Y_L, tmpw
    sbc Y_H, r30                     ; ... but that requires key to not straddle a 256-byte boundary
    eorldq dst, -Y, r30
start:
    qxlat dst, tmpw+1, <7,4,6,5>
    lsl tmpw+1                       ; the bit pattern is constructed to control the loop
    br cc loop
    .if KEY_SIZE > 128
    br hs loop
    .if KEY_SIZE > 192
    lsl tmpw+1
    br ne loop
    .endif
    .endif
    .if ofs
    sbiw Y_L, ofs
    .endif                      
.endif
.endm

.macro round_g_init force=0
.if UNROLL_round_g || force
    .if TAB_sbox == 1
    ldi r31, hi8(twofish_sbox)
    .elseif TAB_q 
    ldi r31, hi8(qbox)
    .else
    ldi r31, hi8(qperm)
    .endif
.endif
.endm

/* uses r0..r3 as working area */
.macro round_g out, src, step=<4+0>
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

; note: we ignore INLINE_round_g=0 in keypair if UNROLL_round_h=1
; or TAB_sbox=1 -- in these cases the keyschedule uses 
; different calls or different round_g functions
; MAYBE: we could still share the MDS computation; but this is
; not a logical thing to want to do in both cases.

.macro keypair kreg, num, pos=0
;.print "keypair kreg, num, pos"
#? Y -> key material
local i, loop, exit, tmp            
.if (UNROLL_keypair || TAB_sbox) && !INLINE_round_g
.warning "Ignoring INLINE_round_g=0 in .macro keypair."
.endif
.if UNROLL_keypair
    .irp ofs, 0, 4
    quad mov 0, num                   ; OPT if num=0, save 1 
    inc num                           ; OPT not necessary the second time if TAB_key == 0
    round_g kreg+ofs, 0, <8+(pos+ofs)>
    .endr
.else
    .if !TAB_key && !INLINE_round_g
    quad push 4                       ; !TAB_key is a annoying option in this case: can't avoid stowing this.
    .endif
    tmp = 4*!INLINE_round_g           ; use r4 as source, to mimick round_F in case !INLINE
loop:
    movq kreg, kreg+4                 ; no-op on the first pass
    quad mov tmp, num  
    inc num
    .if INLINE_round_g || TAB_sbox
    round_g kreg+4, tmp, <8+pos>
    adiw Y_L, 4
    .else
    ldi kreg+4, 4       
    adiw Y_L, pos+KEY_SIZE/16
    shared round_g_rot, %kreg+4, %tmp
	.if !TAB_key
	movq tmp+7, tmp               ; partly undo the last swap
	.endif
    .if pos
    sbiw Y_L, pos-4
    .else
    adiw Y_L, 4
    .endif
    .endif
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
;.print "keypair_wrap kreg, num"
    round_g_init
    keypair kreg, num, KEY_SIZE/16
.endm

/*
 * Y -> pointer to *end* of master key
 *      (really, this makes a lot of sense)
 * X -> holding area for key material (if !STATIC)
 * ---
 * Y -> holding area for key material
 *
 * All keys are stored in reverse order.
 */

; MAYBE: save SIZE/16 bytes of SRAM if TAB_sbox?
twofish_key:
    TAB_sbox = TAB_sbox*2   ; inside this function, always use the original round_g
.if STATIC
    la X, twofish_roundkeys
.endif
    ldi r20, KEY_SIZE/64
1:  reedsolomon -Y
    .irp i, 0,1,2,3         ; order: stores the last sbox key first
    st X+, i
    .endr
    dec r20
    brne 1b

.if TAB_key
    movw r14, X_L           ; save this position to return it to the caller as Y
    ldi r16, 40
    clr r12                 ; round number
    .if INLINE_round_g
    mov r13, r16
    round_g_init
1:  keypair 16, r12         
    .irp i, 0,1,2,3,6,7,4,5 ; fix the order of 2nd key
    st X+, i+16
    .endr
    cpse r12, r13           ; finally an excuse to use this instruction
    .else
    movw r18, X_L
1:  round_g_init
    keypair 20, r12         ; allocating on r20 means we can optimally share round_g (in some cases)
    movw Z_L, r18           ; ... alas, it also means X gets clobbered. 
    .irp i, 0,1,2,3,6,7,4,5 
    st Z+, i+20
    .endr
    movw r18, Z_L
    cpse r12, r16           ; finally an excuse to use this instruction
    .endif
    rjmp 1b
    movw Y_L, r14
.else
    movw Y_L, X_L
.endif
    sbiw Y_L, KEY_SIZE/16

.if TAB_sbox   		    ; precompute the sboxes.
    la X, twofish_sbox
    clr r20
    .if !UNROLL_round_g || !INLINE_round_g
    round_g_init 1
    .endif
1:  quad mov 0, r20
    round_h 0, 0, <4+0>, 16
    .irp i, 0,1,2           ; distribute the result over the sboxes
    st X, i
    inc X_H
    .endr
    st X+, r3
    subi X_H, 3
    inc r20
    br ne 1b
    TAB_sbox=1
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
;.print "round_F out, in, tmp"
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
    xchgq tmp, tmp+4, 0
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
;.print "swap_halves a"
.if !TAB_key                      ; OPT integrate below
    mov r25, r16                  ; this is necessary since we steal r16
    pop r16
    push r8
    mov r8, r25
.endif
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

.if TAB_sbox && !TAB_key
    .error "Incompatible: TAB_sbox = 1 but TAB_key = 0"
    ; we could do this: swap r31 between qbox and sbox constantly
    ; but this just makes the code yet more complicated for no good reason
.endif

.macro whiten_keypair arg=dummy
.if UNROLL_whiten
.error  "Ignoring UNROLL_whiten=1 since TAB_key=0"
.endif
.if !INLINE_whiten
    push r0
.endif
; 19 instructions
    .irp j, 4, 12 
    shared keypair_wrap, 20, r16
    .if j==12
    pop r16
    .endif
    eorq j, 20
    eorq j+6, 24
    .endr
.endm

.macro whiten_tab ofs
.if !UNROLL_whiten
    .irp k, 4,8,12,16
    eorlddq k, Y+k-4+KEY_SIZE/16+ofs
    .endr
.else
    movw Z_L, Y_L
    adiw Z_L, KEY_SIZE/16+ofs
    ldi r20, 16
    la X, 4
1:  ld r1, X
    ld r0, Z+
    eor r1, r0
    st X+, r1
    dec r20
    brne 1b
.endif
.endm

/*
 * Y -> pointer to RS-key + roundkeys (if !STATIC)
 * r4..r19: data to encrypt
 * ---
 * Y -> unchanged (if !STATIC)
 * r4..r19: encrypted block
 */

twofish_enc:

    ; pre-whitening

.if TAB_key
    .if INLINE_whiten
    whiten_tab 0
    .else
    shared whiten_tab 0
    .endif
    movw Z_L, Y_L
    adiw Z_L, KEY_SIZE/16+32
    push Z_H
    push Z_L
.endif
.if !TAB_key
    .if INLINE_whiten
    push r16                    ; round counter overlaps with data; not ideal6
    clr r16
    whiten_keypair r16
    .else
    mov r0, r16
    clr r16
    shared whiten_keypair r16
    .endif
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
    sub Z_L, Y_L                ; deduce round from Y
    cpi Z_L, SCHEDULE_SIZE
.else
    cpi r16, 40
.endif
    br lo, L_enc_loop

.if OMIT_last_swap == UNROLL_enc
    swap_halves 4
.endif

    ; post-whitening
.if TAB_key
    .if INLINE_whiten
    whiten_tab 16
    .else
    adiw Y_L, 16
    shared whiten_tab 0
    sbiw Y_L, 16
    .endif
    pop Z_L
    pop Z_H
.else
    ldi r16, 4
    .if INLINE_whiten
    whiten_keypair r16
    .else
    pop r0
    shared whiten_keypair r16
    .endif
.endif
empty_function:
    ret
    .size twofish_enc, .-twofish_enc

; this is useful if we want the qtable to reside in sram and keep codesize down
; and of course, to test the code
.macro init_q
    .comm qbox, 512, 512
    ldi Z_H, hi8(qperm)
    ldi r17, lo8(qperm)
    ldi Y_H, hi8(qbox)
    clr Y_L
2:  bst Y_H, 0
1:  mov r30, Y_L
    qbox_m r0, r16, r17
    st Y+, r0
    tst Y_L
    brne 1b
    brtc 2b
.endm

.if TAB_q && SRAM_q
twofish_init:
    init_q
    ret
    .size twofish_init, .-twofish_init
.else
twofish_init = empty_function
    .size twofish_init, 0
.endif

FISH_END = .
FISH_PROGSIZE = .-FISH_START

/* tables require alignment so put them at the end */
.text 0x4000

FISH_DATASTART = .

.if TAB_q && !SRAM_q
.p2align 9
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
.else
.p2align 6
qperm:
    ; high nibble: even permutation; low nibble: odd permutation
    ;q0
    .byte 0x8E, 0x1C, 0x7B, 0xD8, 0x61, 0xF2, 0x33, 0x25, 0x0F, 0xB4, 0x5A, 0x96, 0xE7, 0xC0, 0xA9, 0x4D
    .byte 0xBD, 0xA7, 0x5F, 0xE4, 0x61, 0xD2, 0x96, 0x0E, 0xC9, 0x8B, 0xF3, 0x30, 0x28, 0x45, 0x7C, 0x1A

    ;q1
    .byte 0x21, 0x8E, 0xB2, 0xDB, 0xF4, 0x7C, 0x63, 0xE7, 0x36, 0x1D, 0x9A, 0x45, 0x0F, 0xA9, 0xC0, 0x58
    .byte 0x4B, 0xC9, 0x75, 0x51, 0x1C, 0x63, 0x9D, 0xAE, 0x06, 0xE4, 0xD7, 0x8F, 0x22, 0xB0, 0x38, 0xFA
.endif

FISH_DATAEND = .
FISH_DATASIZE = .-FISH_DATASTART

FISH_SIZE = .-FISH_START

.if TAB_sbox
.comm twofish_sbox, 1024, 256
.endif
.if STATIC
.comm twofish_roundkeys, SCHEDULE_SIZE
.else
.comm twofish_roundkeys, SCHEDULE_SIZE
.endif
