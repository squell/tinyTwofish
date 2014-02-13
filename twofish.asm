ideal
p486
include "model.asm"

public twofish_init, twofish_keyschedule, twofish_encrypt, twofish_decrypt

extrn MDS: dword: 256*4
extrn Q: byte: 256*2

codeseg

macro pht x, y             ; pseudo-hadamard transform
  add x, y                 ; x' = x+y , y' = 2y+x
  add y, x
endm

macro sbox_mds nx, x, ny, y
local S1, S2, S3, S4, xl, xh, regc
regc substr <x>, 2, 1      ; regc = a/b/c/d for eax/ebx/ecx/edx
xl   catstr regc, <l>      ; xl = al/bl/... etc.
xh   catstr regc, <h>      ; xh = ah/bh/...
regc substr <y>, 2, 1
yl   catstr regc, <l>
yh   catstr regc, <h>
S1 equ edi+0*256*4
S2 equ edi+1*256*4
S3 equ edi+2*256*4
S4 equ edi+3*256*4
  mov bl, xl
  mov nx, [S1+ebx*4]     ; S1[a]
  mov bl, xh
  ror x, 16
  xor nx, [S2+ebx*4]     ; S2[b]
  mov bl, xl
  xor nx, [S3+ebx*4]     ; S3[c]
  mov bl, xh
  ror x, 16
  xor nx, [S4+ebx*4]     ; S4[d]

  mov bl, yl
  mov ny, [S2+ebx*4]     ; S2[b]
  mov bl, yh
  ror y, 16
  xor ny, [S3+ebx*4]     ; S3[c]
  mov bl, yl
  xor ny, [S4+ebx*4]     ; S4[d]
  mov bl, yh
  ror y, 16
  xor ny, [S1+ebx*4]     ; S1[a]
endm

macro twofish_round nl1,l1,l2, t1,t2
  sbox_mds nl1, l1, ebp, l2
  pht nl1, ebp
  add ebp, [si+4]
  add nl1, [si+0]
  add si, 8
  xor nl1, [t1]
  mov [t1], l2
  mov l2, [t2]
  rol l2, 1
  mov [t2], l1
  ror nl1, 1
  xor l2, ebp
endm

proc twofish_encrypt keys: dataptr, plain: dataptr
t1 equ di+256*4*4+0        ; temps necessary because of register exhaustion
t2 equ di+256*4*4+4

  push ds
  les si, [plain]
  push si
  mov ecx, [es:si+ 0]      ; load block
  mov edx, [es:si+ 4]
  mov eax, [es:si+ 8]
  mov ebx, [es:si+12]
  lds si, [keys]
  lea edi, [si+40*4]       ; [di] -> sboxes
  xor ecx, [si+ 0]
  xor edx, [si+ 4]
  xor eax, [si+ 8]
  xor ebx, [si+12]
  add si, 8*4

  mov [t1], eax
  mov [t2], ebx

  xor ebx, ebx

  push 16/2
@@loop:
  twofish_round eax, ecx, edx, t1,t2
  twofish_round ecx, eax, edx, t2,t1
  dec [word esp]
  jnz @@loop
  pop ax

  mov eax, [t1]
  mov ebx, [t2]
  xor eax, [si-40*4+16]
  xor ebx, [si-40*4+20]
  xor ecx, [si-40*4+24]
  xor edx, [si-40*4+28]
  pop di
  pop ds
  mov [es:di+ 0], eax
  mov [es:di+ 4], ebx
  mov [es:di+ 8], ecx
  mov [es:di+12], edx

  ret
endp

 ; decryption ops (uses same opcode ordering for symmetry in performance)

macro twofish_round nl2,l1,l2, t1,t2
  sbox_mds ebp, l1, nl2, l2
  pht ebp, nl2
  add nl2, [si+4]
  add ebp, [si+0]
  sub si, 8
  xor nl2, [t2]
  mov [t2], l1
  mov l1, [t1]
  rol l1, 1
  mov [t1], l2
  ror nl2, 1
  xor l1, ebp
endm

proc twofish_decrypt keys: dataptr, plain: dataptr
t1 equ di+256*4*4+0        ; temps necessary because of register exhaustion
t2 equ di+256*4*4+4

  push ds
  les si, [plain]
  push si
  mov ecx, [es:si+ 0]      ; load block
  mov edx, [es:si+ 4]
  mov eax, [es:si+ 8]
  mov ebx, [es:si+12]
  lds si, [keys]
  lea edi, [si+40*4]       ; [di] -> sboxes
  xor ecx, [si+16]
  xor edx, [si+20]
  xor eax, [si+24]
  xor ebx, [si+28]
  add si, 40*4 - 8

  mov [t1], eax
  mov [t2], ebx

  xor ebx, ebx

  push 16/2
@@loop:
  twofish_round eax, ecx, edx, t1,t2
  twofish_round edx, ecx, eax, t2,t1
  dec [word esp]
  jnz @@loop
  pop ax

  mov eax, [t1]
  mov ebx, [t2]
  xor eax, [si+8-8*4+ 0]   ; "si+8" -> roundkey 1
  xor ebx, [si+8-8*4+ 4]   ; "-8*4" -> whitening keys start
  xor ecx, [si+8-8*4+ 8]
  xor edx, [si+8-8*4+12]
  pop di
  pop ds
  mov [es:di+ 0], eax
  mov [es:di+ 4], ebx
  mov [es:di+ 8], ecx
  mov [es:di+12], edx

  ret
endp

macro mul_by_2 x, gf_poly
local nosub
  shl x, 1
  jnc nosub
  xor x, (gf_poly and 0FFh)
nosub:
endm

macro div_by_2 x, gf_poly
local nosub
  shr x, 1
  jnc nosub
  xor x, (gf_poly shr 1)
nosub:
endm

  RS_Poly  equ 14Dh

 ;  g(x) = x**4 + (a + 1/a) x**3 + a x**2 + (a + 1/a) x + 1
 ;  where a = primitive root (in other words, a = 2. ;)))

macro reedsolomon w1, w0
local do4, do2, end_do
ifdif <w1>, <eax>
  err
endif
  mov ch, 02h              
do2:
  mov cl, 04h
do4:

  rol w1, 8                ; rotate out byte
  mov dl, al
  mov dh, al
  mul_by_2 dl, RS_Poly     ; dl = (a)
  div_by_2 dh, RS_Poly
  xor dh, dl               ; dh = (a+1/a)
  xor ah, dh               ; (a+1/a)x
  shl edx, 16
  xor w1, edx              ; (a+1/a)x**3 + (a)x**2

  dec cl
  jnz do4
  dec ch
  jz end_do
  xor w1, w0
  jmp do2
end_do:
endm

macro qtab reg, K
local LSB
ifidn <reg>, <eax>
  LSB = al
else
  LSB = dl
endif
irp i, <K>
  mov bl, LSB
  mov LSB, [Q+bx+i*100h]
  ror reg, 8
endm
endm

macro mdstab dest, src
local LSB, MSB
ifidn <src>, <eax>
  LSB = al
  MSB = ah
else
  LSB = dl
  MSB = dh
endif
  movzx bx, LSB
  shl bx, 2
  mov dest, [MDS+bx+0*400h]
  movzx bx, MSB
  shl bx, 2
  xor dest, [MDS+bx+1*400h]
  ror src, 16
  movzx bx, LSB
  shl bx, 2
  xor dest, [MDS+bx+2*400h]
  movzx bx, MSB
  shl bx, 2
  xor dest, [MDS+bx+3*400h]
endm

struc list
  M0 dd ?
  M2 dd ?
  M4 dd ?
  M6 dd ?
  M1 dd ?
  M3 dd ?
  M5 dd ?
  M7 dd ?
  S0 dd ?
  S1 dd ?
  S2 dd ?
  S3 dd ?
  Ln db ?
     db ?
ends

proc twofish_keyschedule key: dataptr
local K: list
  les bx, [key]
  push bx

  movzx di, [es:bx]        ; key length selection
  lea cx, [di+7]
  and cl, not 7            ; push length to next multiple of 8
  cmp cl, 16
  jnb @@no_short_key
  mov cl, 16
@@no_short_key:
  mov si, cx
  sub cl, [es:bx]          ; cx = bytes to pad
  shr si, 3                ; si = bitsize/8/8

  inc bx
  add di, bx               ; pad key with zero's to obtain length
  xor al, al
  cld
  rep stosb                

  lea cx, [si-4]           ; K.Ln: -8=128bit; -4=192bit; 0=256bit key
  shl cx, 2
  mov [K.Ln], cl

  mov di, bx               ; es:di -> master key
  push bp
@@copy_loop:
  mov ebx, [es:di+0]       ; copy off to local lists
  mov eax, [es:di+4]
  mov [K.M0], ebx
  mov [K.M1], eax
  reedsolomon eax, ebx
  mov [K.S0], eax
  add di, 8
  add bp, 4                ; uses base pointer to touch all list items
  dec si
  jnz @@copy_loop
  pop bp

  pop di                   ; es:di -> start of key schedule
  xor eax, eax             ; generate round-keys
@@key_loop:
  xor bx, bx               ; pass even half through qperms, xors and MDS.
  lea edx, [eax+01010101h] 
  cmp [K.Ln], -4           ; key-size switch (128/192/256)
  jl @@key128_1
  je @@key192_1
  qtab eax, <1,0,0,1>
  xor eax, [K.M6]
@@key192_1:
  qtab eax, <1,1,0,0>
  xor eax, [K.M4]
@@key128_1:
  qtab eax, <0,1,0,1>
  xor eax, [K.M2]
  qtab eax, <0,0,1,1>
  xor eax, [K.M0]
  mdstab esi, eax

  xor bx, bx               ; odd half
  lea eax, [edx+01010101h] 
  cmp [K.Ln], -4           ; key-size switch (128/192/256)
  jl @@key128_2
  je @@key192_2
  qtab edx, <1,0,0,1>
  xor edx, [K.M7]
@@key192_2:
  qtab edx, <1,1,0,0>
  xor edx, [K.M5]
@@key128_2:
  qtab edx, <0,1,0,1>
  xor edx, [K.M3]
  qtab edx, <0,0,1,1>
  xor edx, [K.M1]
  mdstab ecx, edx

  rol ecx, 8               ; rotations and PHT
  pht esi, ecx
  rol ecx, 9

  mov [es:di+0], esi       ; save
  mov [es:di+4], ecx
  add di, 8
  cmp al, 40               ; generate 40 dwords (16 rounds*2 + 2*4 whitening)
  jb @@key_loop

  xor eax, eax             ; generate sboxes (through mds)
  lea esi, [MDS]
  movsx cx, [K.Ln]
  add bp, cx               ; pop bp back for smaller keys for proper fit
@@box_loop:                ; (so S3 can become S2 or S1 in the following)
  xor bx, bx
  lea edx, [eax+01010101h]
  cmp cl, -4               ; key-size switch (128/192/256)
  jl @@box128
  je @@box192
  qtab eax, <1,0,0,1>
  xor eax, [K.S0]
@@box192:
  qtab eax, <1,1,0,0>
  xor eax, [K.S1]
@@box128:
  qtab eax, <0,1,0,1>
  xor eax, [K.S2]
  qtab eax, <0,0,1,1>
  xor eax, [K.S3]
  irp i, <0,1,2,3>
    movzx ebx, al
    ror eax, 8
    mov ebx, [esi+(ebx+i*100h)*4]
    mov [es:di+(i*100h)*4], ebx
  endm
  add di, 4
  mov eax, edx
  test al, al              ; go through all 256 values
  jnz @@box_loop
  sub bp, cx               ; undo popback

  lea di, [K]              ; zero out temporaries
  mov cx, 8+4              
  mov bx, ss
  mov es, bx
  rep stosd
  stosb
  ret
endp

  MDS_Poly equ 169h

macro make_column val, coef
irp x, <coef>
  ifidn     <x>,<_01>
    mov al, val
    ror eax, 8
  elseifidn <x>,<_5B>     ; *5B = *5A + 1
    mov al, val
    div_by_2 al, MDS_Poly ; 5A is the inverse of 2 over this gf(2^8)
    div_by_2 al, MDS_Poly
    xor al, val
    ror eax, 8
  elseifidn <x>,<_EF>     ; *EF = *EE + 1 = *5A + *B4 + 1
    mov dl, val
    div_by_2 dl, MDS_Poly
    mov al, dl
    div_by_2 dl, MDS_Poly ; B4 = inverse of 4
    xor al, dl
    xor al, val
    ror eax, 8
  else
    err
  endif
endm
endm

proc twofish_init         ; note: MDS matrix is mirrored into a "proper"
  lea di, [MDS]           ; form, effectively becoming the second parameter
  mov bx, ds              ; with the input vector ordered horizontally
  mov es, bx              ; + because the MDS matrix always takes the
  cld                     ; input of the last layer of Q-permutations,
  xor bx, bx              ; they're added here allready
@@loop1:
  make_column [Q+bx+100h], <_01,_5B,_EF,_EF>
  stosd
  inc bl
  jnz @@loop1
@@loop2:
  make_column [Q+bx],      <_EF,_EF,_5B,_01>
  stosd
  inc bl
  jnz @@loop2
@@loop3:
  make_column [Q+bx+100h], <_5B,_EF,_01,_EF>
  stosd
  inc bl
  jnz @@loop3
@@loop4:
  make_column [Q+bx],      <_5B,_01,_EF,_5B>
  stosd
  inc bl
  jnz @@loop4
  ret
endp

end

 ; [2001-112] byte-byte identical

 ; Last fiddled: [2001-166]

