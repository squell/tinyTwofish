/*
  tinyTwofish implementation for tinyAVR (ATtiny25/45/85)

  Wrapper for the (gcc) C api -- handles initialization

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

.include "2fish_avr.cfg"
.include "avrmacros.s"

.section .text

.global twofish_call_saver, twofish_enc_trampoline

;? Z -> actual procedure to call
twofish_call_saver:
    .irp reg, 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,28,29
    push r\reg
    .endr
    movw Y_L, X_L
    icall
    .irp reg, 29,28,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2
    pop r\reg
    .endr
    clr r1
    ret

;! copy the argument (in X) to r4..r19, then copy back
twofish_enc_trampoline:
    push X_L
    push X_H
    .irp i, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
    ld 4+\i, X+
    .endr

    rcall twofish_enc

    pop X_H
    pop X_L
    .irp i, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
    st X+, 4+\i
    .endr
    ret
