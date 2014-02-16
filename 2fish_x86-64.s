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

.global twofish_init, twofish_key, twofish_enc

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
round_g:          # pass eax through the sboxes
    mov dil, 4
1:  movzx ebx, al
    mov al, [rcx+rbx*4]
    ror eax, 8
    inc rcx
    dec dil
    jnz 1b
    sub rcx, 4    # this loop has a (relatively) huge overhead. :)

/* the following code worked on the first attempt; which just proves that 
   asm forces you to pay attention, and paying attention means less bugs,
   therefore programming in asm means less bugs. */

# in: ax, out: di, clobbers: r11,r10,bx
mds:
    mov r11d, 0x357cd3cd # encoding of the matrix
    xor edi, edi

2:  xor dil, al
    mov bl, al
    gf_shr bl, 0x169 # we could free up one register by using bh as tmp
    shl r11d, 1      # (but that is a bad idea)
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

#void twofish_enc(void *dest, void const *src, schedule const, sbox const)
twofish_enc:
    push r12
    push rbx
    push rdi
# rdi,rsi -> dst,src
# rdx,rcx -> roundkeys, sbox

    mov r8, [rsi]
    mov r9, [rsi+8] # rsi free.

    xor r8, [rdx]
    xor r9, [rdx+8]

    mov esi, 16
    lea rdx, [rdx+(rsi+4)*8]
    neg rsi

1:  round_F r9, r8, [rdx+rsi*8]
    xchg r8, r9
    inc rsi
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
    mov dil, 4
1:  ror r11d, 1
    setc bh
    mov bl, al
    mov al, [qbox+rbx]
    ror eax, 8
    dec dil
    jnz 1b
    ret

# reduces edx:eax, clobbers: cx, bx ax, returns: edx
.macro reedsolomon poly
#  g(x) = x**4 + (a + 1/a) x**3 + a x**2 + (a + 1/a) x + 1
local outer, inner
    mov cl, 0x88             # you should know what this is for by now
    xor edx, eax
outer:
    xor edx, eax
inner:
    rol edx, 8               # rotate out byte
    mov bh, dl
    gf_shr bh, 0x14D, ch     # bh = (1/a)
    mov bl, dl
    gf_shl bl, 0x14D, ch     # bl = (a)
    xor bh, bl               # bh = (a+1/a)
    xor dh, bh               # (a+1/a)x
    shl ebx, 16
    xor edx, ebx             # (a+1/a)x**3 + (a)x**2

    shr cl, 1
    jnc inner
    jnz outer
.endm

# void twofish_key(int bits, byte const *master_key, schedule keys, sbox sbox)
twofish_key:
# edi,rsi -> bits,masterkey
# rdx,rcx -> roundkeys, sbox
    push r12
    push rbx
    push rcx

    # compute round-keys first
    shr edi, 6
    lea r8, [(rdi+1)*4]   # free: rdi
    xor r9d, r9d

1:  call round_h
    mov r12d, edi
    add rsi, 4
    call round_h
    sub rsi, 4

    rol edi, 8
    pht r12d, edi
    rol edi, 9
    movzx rbx, r9b
    mov [rdx+(rbx-2)*4], r12d
    mov [rdx+(rbx-2)*4+4], edi

    cmp r9b, 40
    jb 1b

    # round keys computed
    pop r12 # -> sboxes

    mov rcx, r8
    mov r11d, qselector
    rol r11d, cl
    lea rsi, [rsi+(r8-4)*2] # pre-adjust rsi
    neg r8

    # going to compute the sboxes incrementally, so first initialize
    xor r9d, r9d
2:  movzx ebx, r9b
    mov [r12+rbx*4], r9d
    add r9d, 0x01010101
    jnc 2b

    # jump-start the h-round loop
    xor edx, edx
    jmp 3f

    # for each RS-codeword, apply a step of h
1:  shr r11d, 4
    mov eax, [rsi+r8*2]
    mov edx, [rsi+r8*2+4]
    reedsolomon 0x14D

3:  xor r9d, r9d
2:  movzx ebx, r9b
    mov eax, [r12+rbx*4]
    xor eax, edx
    call round_h_step
    rol r11d, 4

    movzx ebx, r9b
    mov [r12+rbx*4], eax
    add r9d, 0x01010101
    jnc 2b

    add r8, 4
    jnz 1b

    pop rbx
    pop r12
    ret

# there's no point in pre-computing anything -- the qbox will still take up memory.
twofish_init:
    ret

.data

qbox:
    .byte 0xa9, 0x67, 0xb3, 0xe8, 0x04, 0xfd, 0xa3, 0x76, 0x9a, 0x92, 0x80, 0x78, 0xe4, 0xdd, 0xd1, 0x38, 0x0d, 0xc6, 0x35, 0x98, 0x18, 0xf7, 0xec, 0x6c, 0x43, 0x75, 0x37, 0x26, 0xfa, 0x13, 0x94, 0x48, 0xf2, 0xd0, 0x8b, 0x30, 0x84, 0x54, 0xdf, 0x23, 0x19, 0x5b, 0x3d, 0x59, 0xf3, 0xae, 0xa2, 0x82, 0x63, 0x01, 0x83, 0x2e, 0xd9, 0x51, 0x9b, 0x7c, 0xa6, 0xeb, 0xa5, 0xbe, 0x16, 0x0c, 0xe3, 0x61, 0xc0, 0x8c, 0x3a, 0xf5, 0x73, 0x2c, 0x25, 0x0b, 0xbb, 0x4e, 0x89, 0x6b, 0x53, 0x6a, 0xb4, 0xf1, 0xe1, 0xe6, 0xbd, 0x45, 0xe2, 0xf4, 0xb6, 0x66, 0xcc, 0x95, 0x03, 0x56, 0xd4, 0x1c, 0x1e, 0xd7, 0xfb, 0xc3, 0x8e, 0xb5, 0xe9, 0xcf, 0xbf, 0xba, 0xea, 0x77, 0x39, 0xaf, 0x33, 0xc9, 0x62, 0x71, 0x81, 0x79, 0x09, 0xad, 0x24, 0xcd, 0xf9, 0xd8, 0xe5, 0xc5, 0xb9, 0x4d, 0x44, 0x08, 0x86, 0xe7, 0xa1, 0x1d, 0xaa, 0xed, 0x06, 0x70, 0xb2, 0xd2, 0x41, 0x7b, 0xa0, 0x11, 0x31, 0xc2, 0x27, 0x90, 0x20, 0xf6, 0x60, 0xff, 0x96, 0x5c, 0xb1, 0xab, 0x9e, 0x9c, 0x52, 0x1b, 0x5f, 0x93, 0x0a, 0xef, 0x91, 0x85, 0x49, 0xee, 0x2d, 0x4f, 0x8f, 0x3b, 0x47, 0x87, 0x6d, 0x46, 0xd6, 0x3e, 0x69, 0x64, 0x2a, 0xce, 0xcb, 0x2f, 0xfc, 0x97, 0x05, 0x7a, 0xac, 0x7f, 0xd5, 0x1a, 0x4b, 0x0e, 0xa7, 0x5a, 0x28, 0x14, 0x3f, 0x29, 0x88, 0x3c, 0x4c, 0x02, 0xb8, 0xda, 0xb0, 0x17, 0x55, 0x1f, 0x8a, 0x7d, 0x57, 0xc7, 0x8d, 0x74, 0xb7, 0xc4, 0x9f, 0x72, 0x7e, 0x15, 0x22, 0x12, 0x58, 0x07, 0x99, 0x34, 0x6e, 0x50, 0xde, 0x68, 0x65, 0xbc, 0xdb, 0xf8, 0xc8, 0xa8, 0x2b, 0x40, 0xdc, 0xfe, 0x32, 0xa4, 0xca, 0x10, 0x21, 0xf0, 0xd3, 0x5d, 0x0f, 0x00, 0x6f, 0x9d, 0x36, 0x42, 0x4a, 0x5e, 0xc1, 0xe0

    .byte 0x75, 0xf3, 0xc6, 0xf4, 0xdb, 0x7b, 0xfb, 0xc8, 0x4a, 0xd3, 0xe6, 0x6b, 0x45, 0x7d, 0xe8, 0x4b, 0xd6, 0x32, 0xd8, 0xfd, 0x37, 0x71, 0xf1, 0xe1, 0x30, 0x0f, 0xf8, 0x1b, 0x87, 0xfa, 0x06, 0x3f, 0x5e, 0xba, 0xae, 0x5b, 0x8a, 0x00, 0xbc, 0x9d, 0x6d, 0xc1, 0xb1, 0x0e, 0x80, 0x5d, 0xd2, 0xd5, 0xa0, 0x84, 0x07, 0x14, 0xb5, 0x90, 0x2c, 0xa3, 0xb2, 0x73, 0x4c, 0x54, 0x92, 0x74, 0x36, 0x51, 0x38, 0xb0, 0xbd, 0x5a, 0xfc, 0x60, 0x62, 0x96, 0x6c, 0x42, 0xf7, 0x10, 0x7c, 0x28, 0x27, 0x8c, 0x13, 0x95, 0x9c, 0xc7, 0x24, 0x46, 0x3b, 0x70, 0xca, 0xe3, 0x85, 0xcb, 0x11, 0xd0, 0x93, 0xb8, 0xa6, 0x83, 0x20, 0xff, 0x9f, 0x77, 0xc3, 0xcc, 0x03, 0x6f, 0x08, 0xbf, 0x40, 0xe7, 0x2b, 0xe2, 0x79, 0x0c, 0xaa, 0x82, 0x41, 0x3a, 0xea, 0xb9, 0xe4, 0x9a, 0xa4, 0x97, 0x7e, 0xda, 0x7a, 0x17, 0x66, 0x94, 0xa1, 0x1d, 0x3d, 0xf0, 0xde, 0xb3, 0x0b, 0x72, 0xa7, 0x1c, 0xef, 0xd1, 0x53, 0x3e, 0x8f, 0x33, 0x26, 0x5f, 0xec, 0x76, 0x2a, 0x49, 0x81, 0x88, 0xee, 0x21, 0xc4, 0x1a, 0xeb, 0xd9, 0xc5, 0x39, 0x99, 0xcd, 0xad, 0x31, 0x8b, 0x01, 0x18, 0x23, 0xdd, 0x1f, 0x4e, 0x2d, 0xf9, 0x48, 0x4f, 0xf2, 0x65, 0x8e, 0x78, 0x5c, 0x58, 0x19, 0x8d, 0xe5, 0x98, 0x57, 0x67, 0x7f, 0x05, 0x64, 0xaf, 0x63, 0xb6, 0xfe, 0xf5, 0xb7, 0x3c, 0xa5, 0xce, 0xe9, 0x68, 0x44, 0xe0, 0x4d, 0x43, 0x69, 0x29, 0x2e, 0xac, 0x15, 0x59, 0xa8, 0x0a, 0x9e, 0x6e, 0x47, 0xdf, 0x34, 0x35, 0x6a, 0xcf, 0xdc, 0x22, 0xc9, 0xc0, 0x9b, 0x89, 0xd4, 0xed, 0xab, 0x12, 0xa2, 0x0d, 0x52, 0xbb, 0x02, 0x2f, 0xa9, 0xd7, 0x61, 0x1e, 0xb4, 0x50, 0x04, 0xf6, 0xc2, 0x16, 0x25, 0x86, 0x56, 0x55, 0x09, 0xbe, 0x91
