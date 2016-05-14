/*
  tinyTwofish implementation for AVR
  Interface for C programs

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

USAGE:

Link with 2fish_avr.o and 2fish_c_avr.o

This file defines the following macro:

twofish_encrypt(unsigned char data[16]);
    Encrypt a single data block.

EXAMPLE:

#include "2fish_c_avr.h"

char data[16];

int main(void)
{
    twofish_encrypt(data);
    asm("cli");
    asm("sleep");
}

*/

#define twofish_encrypt(data) { \
    register void *clobber; \
    asm volatile("rcall twofish_call_saver" : "=x"(clobber), "=z"(clobber) : "z"(twofish_enc_trampoline), "x"(data) \
        : "r0", "r18", "r19", "r20", "r21", "r22", "r23", "r24", "r25" ); \
    }

/* do not call these functions from C code */
extern twofish_enc_trampoline();

