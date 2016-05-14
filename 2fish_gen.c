/*

 Generate the key material for a tinyTwofish with hard-coded key material

 copyright (c) 2016 marc schoolderman

 permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is
 hereby granted, provided that the above copyright notice and this permission notice appear in all copies.

 */

#include <stdio.h>
#include <string.h>
#include "2fishy.h"

/* enter the key here */
#define KEY_SIZE 128
unsigned char const key[KEY_SIZE/8] = { 
        0 
};

void printwords(int n, word *buf)
{
    word x;
    printf("\t.byte ");
    while(n-- > 1) {
        x = *buf++;
        printf("0x%02x, 0x%02x, 0x%02x, 0x%02x, ", x&0xFF, (x>>8)&0xFF, (x>>16)&0xFF, (x>>24)&0xFF);
    }
    x = *buf++;
    printf("0x%02x, 0x%02x, 0x%02x, 0x%02x\n", x&0xFF, (x>>8)&0xFF, (x>>16)&0xFF, (x>>24)&0xFF);
}

void printbytes(int n, byte *buf)
{
    printf("\t.byte ");
    while(n-- > 1) {
        printf("0x%02x, ", *buf++);
    }
    printf("0x%02x\n", *buf++);
}

int main(void)
{
    sbox sbox;
    schedule schedule;
    int i;

    twofish_init();
    twofish_key(KEY_SIZE, key, schedule, sbox);

    printf("twofish_roundkeys:\n");
    for(i=0; i < 10; i++) {
        printwords(4, schedule+i*4);
    }
    printf(".size twofish_roundkeys, .-twofish_roundkeys\n");

    printf(".text 0x4000\n");
    printf(".p2align 8\n");
    printf("twofish_sbox:\n");
    for(i=0; i < 4*256/16; i++) {
        printbytes(16, sbox+i*16);
    }
    printf(".size twofish_sbox, .-twofish_sbox\n");

    return 0;
}

