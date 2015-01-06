/*
  tinyTwofish implementation for tinyAVR (ATtiny25/45/85)

  KAT tests

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

.global kat

.section .bss
katkey:  .space KEY_SIZE/8
katdata: .space 16
schedule:.space SCHEDULE_SIZE

.section .text

    rcall twofish_init
kat:
    clr r20                           ; setup
    la Y, katdata
    setmem Y, 16, r20, r21
    la Y, katkey
    setmem Y, KEY_SIZE/8, r20, r21

1:  push r20
    la X, schedule
    .if TAB_key
    la Y, katkey+KEY_SIZE/8
    .else
    la Y, schedule+KEY_SIZE/16
    loadram Y, katkey, KEY_SIZE/8, ld
    .endif
    rcall twofish_key                    ; produce twofish_roundkeys

    copy katkey, katkey+16, KEY_SIZE/8-16; update the masterkey
    copy katdata, katkey, 16

    la X, 4                              ; encrypt
    loadram X, katdata, 16, ld
    la Y, schedule
    rcall twofish_enc

    la X, katdata                       ; save result and loop
    loadram X, 4, 16, ld
    pop r20
    inc r20
    cpi r20, 49
    brlo 1b

    cli
    sleep

; suppress double allocation of roundkeys by 2fish_avr.s if STATIC=1
.global twofish_roundkeys
twofish_roundkeys = schedule
