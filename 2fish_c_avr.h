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

This file defines three macro's, which you can use as-if

const unsigned int twofish_keysize;   
    A constant telling you what key-size tinyTwofish was assembled with

twofish_setkey(const unsigned char master_key[twofish_keysize/8]);
    Sets the Twofish key to `master_key'.

twofish_encrypt(unsigned char data[16]);
    Encrypt a single data block.

EXAMPLE:

#include "2fish_c_avr.h"

char mkey[16];

int main(void)
{
    twofish_setkey(mkey);
    twofish_encrypt(mkey);
    asm("cli");
    asm("sleep");
}

*/

/* not a compile-time constant, but better than nothing */
#define twofish_keysize (2*(unsigned int)twofish_keysize_trampoline)

/* work arounds are necessary to tell avr-gcc that inputs are clobbered as well */
#define twofish_setkey(master_key) { \
    register void *clobber; \
    asm volatile("rcall twofish_call_saver" : "=x"(clobber), "=z"(clobber) : "z"(twofish_key), "x"(&master_key[sizeof master_key]) \
        : "r0", "r18", "r19", "r20", "r21", "r22", "r23", "r24", "r25"); \
    }

#define twofish_encrypt(data) { \
    register void *clobber; \
    asm volatile("rcall twofish_call_saver" : "=x"(clobber), "=z"(clobber) : "z"(twofish_enc_trampoline), "x"(data) \
        : "r0", "r18", "r19", "r20", "r21", "r22", "r23", "r24", "r25" ); \
    }

/* do not call these functions from C code */
extern twofish_key();
extern twofish_enc_trampoline();
extern twofish_keysize_trampoline();

