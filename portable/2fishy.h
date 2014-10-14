/*

 Quick and dirty but portable reference implementation of Twofish;
 Sticking uncomfortably close to the definition.

 copyright (c) 2014 marc schoolderman

 permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is
 hereby granted, provided that the above copyright notice and this permission notice appear in all copies.

 */

#ifndef INCLUDED_2fishy_h
#define INCLUDED_2fishy_h

#if __STDC_VERSION__ >= 199901L
#  include <stdint.h>
typedef uint32_t word;
typedef uint8_t byte;
typedef uint_fast16_t hword;
#else
#  include <limits.h>
#  if UINT_MAX == 0xFFFFFFFFUL
typedef unsigned int word;
#  elif ULONG_MAX == 0xFFFFFFFFUL
typedef unsigned long int word;
#  else
#    error Your computer architecture is too exotic.
typedef void* word;
#  endif
typedef unsigned char byte;
typedef unsigned short int hword;
typedef int static_assert_words_are_32bit[sizeof(word)==4?1:-1];
#endif

typedef word schedule[8+2*16];
#ifdef SBOX32
typedef word sbox[4*256];
#else
typedef byte sbox[4*256];
#endif

void twofish_init(void);
void twofish_key(int bits, byte const *master_key, schedule, sbox);
void twofish_enc(void *dest, void const *src, schedule const, sbox const);

#endif
