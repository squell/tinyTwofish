/*

 Reproduced the Test vectors from the Twofish paper.

 copyright (c) 2014 marc schoolderman

 permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is
 hereby granted, provided that the above copyright notice and this permission notice appear in all copies.

 */

#include <stdio.h>
#include <string.h>
#include "2fishy.h"

void print(int n, byte *buf)
{
    while(n--) 
	printf("%02X", *buf++);
    printf("\n");
}

void run(int bits) 
{
    sbox sbox;
    schedule key;
    byte buf[2][32];
    int i;

    printf("KEYSIZE=%d\n", bits);
    memset(buf, 0, sizeof buf);
    for(i=1; i<=49; i++) {
	printf("I=%d\n", i);
	printf("KEY=");
	print(bits/8, buf[i%2]);

	twofish_key(bits, buf[i%2], key, sbox);
	memcpy(buf[i%2]+16, buf[(i+1)%2], 16);
	memcpy(buf[i%2],    buf[(i+1)%2], 16);

	printf("PT=");
	print(128/8, buf[i%2]);

	twofish_enc(buf[i%2], key, sbox);
	printf("CT=");
	print(128/8, buf[i%2]);
    }
}

int main(void) 
{
    static byte zero[32];
    static byte k256[32] = {
	0x01,0x23,0x45,0x67,
	0x89,0xAB,0xCD,0xEF,
	0xFE,0xDC,0xBA,0x98,
	0x76,0x54,0x32,0x10,
	0x00,0x11,0x22,0x33,
	0x44,0x55,0x66,0x77,
	0x88,0x99,0xAA,0xBB,
	0xCC,0xDD,0xEE,0xFF
    };

    twofish_init();
    run(128);
    run(192);
    run(256);

    return 0;
}

