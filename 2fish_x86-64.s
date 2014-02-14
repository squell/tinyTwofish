/*
  tinyTwofish implementation for x86-64

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

.intel_syntax noprefix
.altmacro

.text

.macro pht a, b
    add a, b
    add b, a
.endm

.macro cxor dst, src, tmp=r10b
    sbb tmp, tmp
    and tmp, src
    xor dst, tmp
.endm

.macro gf_shl reg, poly, tmp=r10b
    shl reg, 1
    cxor reg, poly&&0xFF, tmp
.endm

.macro gf_shr reg, poly, tmp=r10b
    shr reg, 1
    cxor reg, poly>>1, tmp
.endm

# NOTE: sbox layout is different from the C impl.

# in: ax, out: di, clobbers: r10,r11,bx
round_g:
    movzx ebx, al # pass eax throug the sboxes
    mov al, [rcx+rbx*4]
    movzx ebx, ah
    mov ah, [rcx+rbx*4+1]
    ror eax, 16
    movzx ebx, al
    mov al, [rcx+rbx*4+2]
    movzx ebx, ah
    mov ah, [rcx+rbx*4+3]
    ror eax, 16
    mov edi, eax

/* the following code worked on the first attempt; which just proves that 
   asm forces you to pay attention, and paying attention means less bugs,
   therefore programming in asm means less bugs. */

# in: ax, out: di, clobbers: r11,r10,bx
mds:
    mov r11d, 0x357cd3cd # encoding of the matrix
    xor edi, edi

2:  xor dil, al
    mov bl, al
    gf_shr bl, 0x169
    shl r11d, 1
    cxor dil, bl
    gf_shr bl, 0x169
    shl r11d, 1
    cxor dil, bl
    ror eax, 8

    test r11b, r11b
    jnz 2b

    ror edi, 8
    dec r11b

    cmp r11d, -1
    jne 2b

    ret

.macro round_F dst, src, key
    mov eax, src&d
    call round_g
    mov r12d, edi
    mov rax, src
    shr rax, 32
    rol eax, 8
    call round_g
    pht r12d, edi
    add r12d, [key]
    add edi,  [key+4]
    xor r12d, dst&d
    ror r12d, 1
    shr dst, 32
    rol dst&d, 1
    xor dst&d, edi
    shl dst, 32
    or dst, r12
.endm

.global twofish_enk

#void twofish_enc(void *dest, void const *src, schedule const, sbox const)
twofish_enk:
    push r12
    push rbx
    push rdi
# rdi,rsi -> dst,src
# rdx,rcx -> roundkeys, sbox

    mov r8, [rsi]
    mov r9, [rsi+8] # rsi free.

    xor r8, [rdx]
    xor r9, [rdx+8]

    mov rsi, -16*8
    sub rdx, rsi
    add rdx, 32

1:  round_F r9, r8, [rdx+rsi]
    xchg r8, r9
    add rsi, 8
    jnz 1b

    xor r9, [rdx-32*4-16]
    xor r8, [rdx-32*4- 8]

    pop rdi
    mov [rdi], r9
    mov [rdi+8], r8
    pop rbx
    pop r12
    ret


# control word for the qboxes
.equ qselector, 0x5ca39000

# expects: word++ in r9d; r8: count rsi->key material; ecx: clobbers: ebx, r0, r11, rcx, rax
round_h:
    mov eax, r9d
    add r9d, 0x01010101
    mov rcx, r8
    mov r11d, qselector
    rol r11d, cl
    xor rbx, rbx

2:  call round_h_step
    sub rcx, 4
    jz mds

    xor eax, [rsi+(rcx-4)*2]
    jmp 2b

# passes a word through a series of qboxes
# expects: rbx upper parts zero; r11d: control word
round_h_step:
    ror r11d, 1
    setc bh
    mov bl, al
    mov al, [qbox+rbx]
    ror r11d, 1
    setc bh
    mov bl, ah
    mov ah, [qbox+rbx]
    ror eax, 16

    ror r11d, 1
    setc bh
    mov bl, al
    mov al, [qbox+rbx]
    ror r11d, 1
    setc bh
    mov bl, ah
    mov ah, [qbox+rbx]
    ror eax, 16

    ret

# reduces edx:eax, clobbers: cx, r10, ebx
.macro reedsolomon poly
#  g(x) = x**4 + (a + 1/a) x**3 + a x**2 + (a + 1/a) x + 1
local outer, inner
    mov cl, 0x88             # you should know what this is for by now
    xor eax, edx             
    xor edx, eax
outer:
    xor eax, edx
inner:
    rol eax, 8               # rotate out byte
    mov bh, al
    gf_shr bh, 0x14D, bl     # bh = (1/a)
    mov bl, al
    gf_shl bl, 0x14D         # bl = (a)
    xor bh, bl               # bh = (a+1/a)
    xor ah, bh               # (a+1/a)x
    shl ebx, 16
    xor eax, ebx             # (a+1/a)x**3 + (a)x**2

    shr cl, 1
    jnc inner
    jnz outer
.endm

.global twofish_kei

# void twofish_key(int bits, byte const *master_key, schedule keys, sbox sbox)
twofish_kei:
# edi,rsi -> bits,masterkey
# rdx,rcx -> roundkeys, sbox
    push rbp
    push rbx
    push rcx

    # compute round-keys first
    shr edi, 6
    lea r8, [(rdi+1)*4]   # free: rdi
    xor r9d, r9d

1:  call round_h
    mov ebp, edi
    add rsi, 4
    call round_h
    sub rsi, 4

    rol edi, 8
    pht ebp, edi
    rol edi, 9
    movzx rbx, r9b       # note: rbx upper bits zero'd for reedsolomon
    mov [rdx+(rbx-2)*4], ebp
    mov [rdx+(rbx-2)*4+4], edi

    cmp r9b, 40
    jb 1b

    # round keys computed
    pop rbp # -> sboxes

    mov rcx, r8
    sub r8, 4
    mov r11d, qselector
    rol r11d, cl
    lea rsi, [rsi+r8*2] # pre-adjust rsi
    neg r8

    # going to compute the sboxes incrementally, so first initialize
    xor r9d, r9d
2:  mov eax, r9d
    call round_h_step
    rol r11d, 4
    movzx ebx, r9b
    mov [rbp+rbx*4], eax
    add r9d, 0x01010101 
    jnc 2b

    # now, for each RS-cod:
1:  shr r11d, 4
    mov eax, [rsi+r8*2]
    mov edx, [rsi+r8*2+4]
    reedsolomon 0x14D
    mov r10d, eax

    xor r9d, r9d
2:  movzx ebx, r9b
    mov eax, [rbp+rbx*4]
    xor eax, r10d
    call round_h_step
    rol r11d, 4

    movzx ebx, r9b
    mov [rbp+rbx*4], eax
    add r9d, 0x01010101
    jnc 2b

    add r8, 4
    jnz 1b

    pop rbx
    pop rbp
    ret

