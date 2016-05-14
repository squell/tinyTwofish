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
    la Z, 4				; set r4 .. r19 (plaintext) to zero
    clr r0
    setmem Z, 16, r0, r20

    rcall twofish_enc

    cli
    sleep
    .size startup, .-startup

.data
