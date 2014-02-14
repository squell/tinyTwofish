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
    cxor reg, poly&0xFF, tmp
.endm

.macro gf_shr reg, poly, tmp=r10b
    shr reg, 1
    cxor reg, poly>>1, tmp
.endm

# in: ax, out: di, clobbers: r10,r11,bx
round_g:
    movzx ebx, al # pass eax throug the sboxes
    mov al, [rcx+0x000+rbx]
    movzx ebx, ah
    mov ah, [rcx+0x100+rbx]
    ror eax, 16
    movzx ebx, al
    mov al, [rcx+0x200+rbx]
    movzx ebx, ah
    mov ah, [rcx+0x300+rbx]
    ror eax, 16
    mov edi, eax

/* the following code worked on the first attempt; which just proves that 
   asm forces you to pay attention, and paying attention means less bugs,
   therefore programming in asm means less bugs. */

# in: ax, out: di, clobbers: r11,bx
mds:
    mov r11d, 0x357cd3cd # just go with it.
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
    mov eax, src\()d
    call round_g
    mov r12d, edi
    mov rax, src
    shr rax, 32
    rol eax, 8
    call round_g
    pht r12d, edi
    add r12d, [key]
    add edi,  [key+4]
    xor r12d, dst\()d
    ror r12d, 1
    shr dst, 32
    rol dst\()d, 1
    xor dst\()d, edi
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

    mov rsi, -16*2*4
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

