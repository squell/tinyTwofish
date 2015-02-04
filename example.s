/*
  tinyTwofish implementation for tinyAVR (ATtiny25/45/85)

  Example program

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

.include "2fish_avr.cfg"
.include "avrmacros.s"

.text 
startup:
    cli
    rcall twofish_init
    .if STATE < 1 
    sleep
    .endif

    init_data
    .if TAB_key
    la Y, mkey+KEY_SIZE/8		; mkey is a symbol in SRAM
    .else
    la Y, twofish_roundkeys+MASTERKEY_OFS
    loadram Y, mkey, KEY_SIZE/8		; mkey is a symbol in program memory
    .endif

    la X, twofish_roundkeys
    .if STATE < 2
    sleep
    .endif
    rcall twofish_key
    .if STATE < 3
    sleep
    .endif

    la Z, 4				; set r4 .. r19 (plaintext) to zero
    clr r0
    setmem Z, 16, r0, r20

    .if STATE < 4
    sleep
    .endif
    rcall twofish_enc

    sleep
    .size startup, .-startup

.if TAB_key
.data
.endif

mkey:
.if KEY_SIZE > 128
    .byte 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef
    .byte 0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10
    .byte 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77
    .byte 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff
.else
    .space 16
.endif

.comm twofish_roundkeys, SCHEDULE_SIZE

