/*
  Useful AVR macros

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

X_L  = 26
X_H  = 27
Y_L  = 28
Y_H  = 29
Z_L  = 30
Z_H  = 31
SP_H = 0x3e
SP_L = 0x3d
SREG = 0x3f

; flags, for use with brbs
CF=0
ZF=1
NF=2
VF=3
SF=4
HF=5
TF=6
IF=7

/* note: require all 32bit values to be 4-byte aligned, we can use a 
   wrap-around trick to get 8bit-rotations for free */

.macro quad op reg, arg, range=<0,1,2,3>
.irp i, range
.ifnc <>, <arg>
    op ((reg)&~3)+((reg)+i)%4, arg
.else
    op ((reg)&~3)+((reg)+i)%4
.endif
.endr
.endm

.macro zip op rd, rr, range=<0,1,2,3>
.irp i, range
    op ((rd)&~3)+((rd)+i)%4, ((rr)&~3)+((rr)+i)%4
.endr
.endm

.macro la M, ofs
    ldi M&_L, lo8(ofs)
    ldi M&_H, hi8(ofs)
.endm

; used for data that can reside in flash or sram
.macro load reg, Z, sram
.if sram
    ld reg, Z
.else
    lpm reg, Z
.endif
.endm

.macro far xfer label
    ldi Z_H, pm_hi8(label)
    ldi Z_L, pm_lo8(label)
    i&xfer
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

.macro eorlddq a, M, tmp=r0
.irp i, 0,1,2,3
    ldd tmp, M+i
    eor a+i, tmp
.endr
.endm

/* do some macro hacking to find the directionality */
.macro eorldq a, M, tmp=r0, load=ld
.irpc c, M
.ifc <c>, <+>
    .irp i, 0,1,2,3
	load tmp, M
	eor a+i, tmp
    .endr
    .exitm
.endif
.ifc <c>, <->
    .irp i, 3,2,1,0
	load tmp, M
	eor a+i, tmp
    .endr
    .exitm
.endif
.endr
.endm

.macro addldq a, M, tmp=r0, load=ld
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

.macro ror1q a, tmp=r0
    mov tmp, a
    lsr tmp
.irp i, 3,2,1,0
    ror a+i
.endr
.endm

.macro xchg a, b
    eor a, b
    eor b, a
    eor a, b
.endm

.macro xchgq a, b
.ifnc <a>, <b>
.irp i, 0,1,2,3
    xchg ((a)&~3)+((a)+i)%4, ((b)&~3)+((b)+i)%4
.endr
.endif
.endm

.macro setmem D, size, src, cntr=n/a
.ifc <n/a>, <cntr>
    .rept size
    st D&+, src
    .endr
.else
local cp
    ldi cntr, size
cp: st D&+, src
    dec cntr
    brne cp
.endif
.endm

.macro copy src, dst, size, S=X, D=Y, tmp=r0
local cp
.if size
.if src > dst
    ; copy forwards
    la S, src
    la D, dst
cp: ld tmp, S+
    st D+, tmp
    cpi S&_L, lo8(src+size)
    brne cp
.else
    ; copy backwards
    la S, src+size
    la D, dst+size
cp: ld tmp, -S
    st -D, tmp
    cpi S&_L, lo8(src)
    brne cp
.endif
.endif
.endm

.macro loadram D ,src, size, load=lpm, tmp=r0
local cp
    la Z, src
cp: load tmp, Z+
    st D&+, tmp
    cpi Z_L, lo8(src+size)
    brne cp
.endm

/* A cute hack of the GNU as preprocessor: let the assembler, in the case of
 * backjumps, figure out what sequence of opcodes to emit.
 * Can only be used for backjumps since gnu as is single-pass 
 */
.macro br cnd, label
.if .-label < 128
    br&cnd label
.else
local skip, select
select=0
.irp case, lt,ge,lt,eq,ne,eq,lo,sh,cs,cc,cs,mi,pl,mi,vs,vc,vs,tc,ts,tc,hc,hs,hc,id,ie,id
    .if select
    br\case skip
    rjmp label
    .exitm
    .endif
    .ifc <cnd>, <case>
    select=1
    .endif
.endr
skip:
.endif
.endm

/* Use the assembler to perform a sort of crude 'template' instantiation 
 * getting this to be re-entrant is a bit of a hassle...
 */
shared_subsection = 1024
.macro shared op, a=,b,c=,d=
.ifndef I&op&_&a&_&b&_&c&_&d&
.pushsection .text             ; push a dummy section (wont accept an 'equ' here)
.subsection shared_subsection  ; but it will accept it here?! 
shared_subsection=shared_subsection+1
I&op&_&a&_&b&_&c&_&d&:
    .ifc <c>, <>
    op a, b
    .else
    op a, b, c, d
    .endif
    ret
    .size I&op&_&a&_&b&_&c&_&d&, .-I&op&_&a&_&b&_&c&_&d&
.previous
.popsection
.endif
    rcall I&op&_&a&_&b&_&c&_&d&
.endm

.macro dump load org
;.print "dump load org"
local i
    la Z, org
    i=0
    .rept 30
    load i, Z+
    i=i+1
    .endr
    ser r31
.endm

