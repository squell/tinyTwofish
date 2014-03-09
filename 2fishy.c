/*

 Quick and dirty but portable reference implementation of Twofish;
 Sticking uncomfortably close to the definition.

 copyright (c) 2014 marc schoolderman

 permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is
 hereby granted, provided that the above copyright notice and this permission notice appear in all copies.

 */

#include "2fishy.h"
#include <string.h>

#define MATH_OPT

typedef union { byte byte[4]; word word; } vector;

/* on intel, gcc optimizes all this away */
static int little_endian() 
{
    union { word word; byte byte; } tmp = { 1 };
    return tmp.byte;
}

#define swap(type, a, b) { \
    type tmp = a; \
    a = b, b = tmp; \
}

/*
 * Cryptographical primitives
 */

static byte gf_mul(hword x, hword y, hword poly)
{
    /* straight-forward way to perform multiplication in gf2^8 */
    hword acc = 0;
    for( ; y; x <<= 1, y >>= 1) {
        if(x > (x^poly))
	    x ^= poly;
	if(y&1)
	    acc ^= x;
    }
    return acc;
}

static byte gf_shl(hword x, hword poly)
{
    return x<<1 ^ (x&0x80? poly : 0);
}

static byte gf_shr(hword x, hword poly)
{
    return (x&1? x^poly : x) >> 1;
}

/* the only part where endianness really matters is in the matrix mult's;
   we have to mirror the matrix (vertically and horizontally). in all other
   parts the issues sort themselves out. */

#define gf_matrix_mul(dst, M, x, poly, mdir, ndir) { \
    int const rows = sizeof M/sizeof *M; \
    int const cols = sizeof *M/sizeof **M; \
    int n, m; \
    for(m=0; m < rows; m++) \
	for(n=0; n < cols; n++) \
	    dst[m] ^= gf_mul(M[mdir?m:rows-1-m][ndir?n:cols-1-n], x[n], poly); \
}

static word rol(word x, int n)
{
    return x<<n | x>>32-n;
}

/*
 * core encryption functions start here 
 */

static void whiten(word *dat, word const *keys)
{
    dat[0] ^= keys[0];
    dat[1] ^= keys[1];
    dat[2] ^= keys[2];
    dat[3] ^= keys[3];
}

static void pht(word *a, word *b)
{
    *a += *b;
    *b += *a;
}


word bswap(word a)
{
    if(little_endian()) return a;
        vector x;
	    x.word = a;
	        swap(byte, x.byte[0], x.byte[3]);
		    swap(byte, x.byte[1], x.byte[2]);
		        return x.word;
			}

static word mds(word w)
{
#ifndef MATH_OPT
    /* this is the dumb way to implement this */
    static byte matrix[4][4] = {
	{ 0x01, 0xEF, 0x5B, 0x5B },
	{ 0x5B, 0xEF, 0xEF, 0x01 },
	{ 0xEF, 0x5B, 0x01, 0xEF },
	{ 0xEF, 0x01, 0xEF, 0x5B }
    };

    vector x, acc = { 0 };
    x.word = w;
    gf_matrix_mul(acc.byte, matrix, x.byte, 0x169, little_endian(), little_endian());
    return acc.word;
#else
    /* compute each element in GF(2^8) just once, then use the fast multiplier */
    vector m01[4] = { { 1,0,0,0 }, { 0,0,0,1 }, { 0,0,1,0 }, { 0,1,0,0 } };
    vector m5B[4] = { { 0,1,0,0 }, { 0,0,1,0 }, { 1,0,0,0 }, { 1,0,0,1 } };
    vector mEF[4] = { { 0,0,1,1 }, { 1,1,0,0 }, { 0,1,0,1 }, { 0,0,1,0 } };
    vector x;
    int n;
    word acc = 0;
    x.word = w;
    for(n=0; n < 4; n++) {
	byte v, x01, x5B, xEF;
	v = x01 = x5B = xEF = x.byte[little_endian()?n:3-n];
	v = gf_shr(v, 0x169);
	xEF ^= v;
	v = gf_shr(v, 0x169);
	xEF ^= v;
	x5B ^= v;
	acc ^= x01*m01[n].word | x5B*m5B[n].word | xEF*mEF[n].word;
    }
    return acc;
#endif
}

static word round_g(word w, sbox const sbox)
{
    vector x;
    x.word = w;
#ifndef SBOX32
    x.byte[0] = sbox[0x000|x.byte[0]];
    x.byte[1] = sbox[0x100|x.byte[1]];
    x.byte[2] = sbox[0x200|x.byte[2]];
    x.byte[3] = sbox[0x300|x.byte[3]];
    return mds(x.word);
#else
    return sbox[0x000|x.byte[0]] ^ sbox[0x100|x.byte[1]] ^
           sbox[0x200|x.byte[2]] ^ sbox[0x300|x.byte[3]];
#endif
}

static void round_F(word *out, word const *in, word const *keys, sbox const sbox)
{
    word x = round_g(in[0], sbox);
    word y = round_g(rol(in[1],8), sbox);
    pht(&x, &y);
    out[0] = rol(out[0] ^ x+keys[0], 31);
    out[1] = rol(out[1], 1) ^ y+keys[1];
}

void twofish_enc(void *dest, void const *src, schedule const keys, sbox const sbox)
{
    word data[4];
    int i;
    memcpy(data, src, sizeof data);

    whiten(data, keys);
    for(i=0; i < 16; i+=2) {
	round_F(data+2, data+0, keys+2*i+ 8, sbox);
	round_F(data+0, data+2, keys+2*i+10, sbox);
    }
    /* "undo" the last swap */
    swap(word, data[0], data[2]);
    swap(word, data[1], data[3]);
    whiten(data, keys+4);

    memcpy(dest, data, sizeof data);
}

/* 
 * Key scheduling code
 */

static word reedsolomon(byte const *x)
{
#ifndef MATH_OPT
    static byte matrix[4][8] = {
	{ 0x01, 0xA4, 0x55, 0x87, 0x5A, 0x58, 0xDB, 0x9E },
	{ 0xA4, 0x56, 0x82, 0xF3, 0x1E, 0xC6, 0x68, 0xE5 },
	{ 0x02, 0xA1, 0xFC, 0xC1, 0x47, 0xAE, 0x3D, 0x19 },
	{ 0xA4, 0x55, 0x87, 0x5A, 0x58, 0xDB, 0x9E, 0x03 }
    };

    vector acc = { 0 };
    gf_matrix_mul(acc.byte, matrix, x, 0x14D, little_endian(), 1);
    return acc.word;
#else
    /* the above magical matrix is 'simply' the residue in a polynomial field
       RS-poly: x**4 + (a+1/a)x**^3 + ax**2 + (a+1/a)x + 1 */
    #define pos(n) (little_endian()?7-k+n:k+n)%4
    vector acc = { 0 };
    int k;
    for(k=0; k < 8; k++) {
	byte top = x[7-k]^acc.byte[pos(0)];
	byte m = gf_shl(top, 0x14D);
	acc.byte[pos(0)]  = top;
	acc.byte[pos(2)] ^= m;
	m ^= gf_shr(top, 0x14D);
	acc.byte[pos(1)] ^= m;
	acc.byte[pos(3)] ^= m;
    }
    return acc.word;
    #undef pos
#endif
}

static byte qbox[2][256];

static word round_h_aux(word w, word const select)
{
    vector x, s;
    s.word = select;
    x.word = w;
    x.byte[0] = qbox[s.byte[0]][x.byte[0]];
    x.byte[1] = qbox[s.byte[1]][x.byte[1]];
    x.byte[2] = qbox[s.byte[2]][x.byte[2]];
    x.byte[3] = qbox[s.byte[3]][x.byte[3]];
    return x.word;
}

static word round_h(word x, int k, word *L)
{
    static word const selector[5] = {
        0x00010001,
        0x01010000,
        0x01000100,
        0x00000101,
        0x01000001
    };
    for( ; k; k--)
        x = round_h_aux(x, selector[k]) ^ L[(k-1)*2];
    return round_h_aux(x, selector[k]);
}

void twofish_key(int bits, byte const *master_key, schedule keys, sbox sbox)
{
    word key_copy[8];
    int n, k = bits/64;

    /* just to be really pedantic about alignment */
    memcpy(key_copy, master_key, bits/8);

    /* compute the roundkeys */
    for(n=0; n < 20; n++) {
	word a = mds(round_h((2*n  )*0x01010101, k, key_copy  ));
	word b = mds(round_h((2*n+1)*0x01010101, k, key_copy+1));
	b = rol(b, 8);
	pht(&a, &b);
	keys[2*n  ] = a;
	keys[2*n+1] = rol(b, 9);
    }

    /* compute the sboxes */
    for(n=0; n < k; n++)
	key_copy[(k-n-1)*2] = reedsolomon(&master_key[n*8]);

    for(n=0; n < 256; n++) {
	vector x;
	x.word = round_h(n*0x01010101, k, key_copy);
#ifndef SBOX32
	sbox[0x000|n] = x.byte[0];
	sbox[0x100|n] = x.byte[1];
	sbox[0x200|n] = x.byte[2];
	sbox[0x300|n] = x.byte[3];
#else
	sbox[0x000|n] = mds(x.word);
	x.byte[0] = 0;
	sbox[0x000|n] ^= sbox[0x100|n] = mds(x.word);
	x.byte[1] = 0;
	sbox[0x100|n] ^= sbox[0x200|n] = mds(x.word);
	x.byte[2] = 0;
	sbox[0x200|n] ^= sbox[0x300|n] = mds(x.word);
#endif
    }
}

/*
 * Code that computes the Q-tables
 */

void twofish_init(void) 
{
    static byte t[2][4][16] = { 
	{
	    0x8, 0x1, 0x7, 0xd, 0x6, 0xf, 0x3, 0x2, 0x0, 0xb, 0x5, 0x9, 0xe, 0xc, 0xa, 0x4,
	    0xe, 0xc, 0xb, 0x8, 0x1, 0x2, 0x3, 0x5, 0xf, 0x4, 0xa, 0x6, 0x7, 0x0, 0x9, 0xd,
	    0xb, 0xa, 0x5, 0xe, 0x6, 0xd, 0x9, 0x0, 0xc, 0x8, 0xf, 0x3, 0x2, 0x4, 0x7, 0x1,
	    0xd, 0x7, 0xf, 0x4, 0x1, 0x2, 0x6, 0xe, 0x9, 0xb, 0x3, 0x0, 0x8, 0x5, 0xc, 0xa 
	}, {
	    0x2, 0x8, 0xb, 0xd, 0xf, 0x7, 0x6, 0xe, 0x3, 0x1, 0x9, 0x4, 0x0, 0xa, 0xc, 0x5,
	    0x1, 0xe, 0x2, 0xb, 0x4, 0xc, 0x3, 0x7, 0x6, 0xd, 0xa, 0x5, 0xf, 0x9, 0x0, 0x8,
	    0x4, 0xc, 0x7, 0x5, 0x1, 0x6, 0x9, 0xa, 0x0, 0xe, 0xd, 0x8, 0x2, 0xb, 0x3, 0xf,
	    0xb, 0x9, 0x5, 0x1, 0xc, 0x3, 0xd, 0xe, 0x6, 0x4, 0x7, 0xf, 0x2, 0x0, 0x8, 0xa
	}
    };
    int n, i;
    for(i=0; i<256; i++) {
	for(n=0; n<2; n++) {
	    byte a,b,x = i;
	    a = (x>>4) ^ (x&0xF);
	    b = (x>>4) ^ (x>>1)&0xF ^ (x<<3)&0x8;
	    x = t[n][0][a] << 4 | t[n][1][b];
	    a = (x>>4) ^ (x&0xF);
	    b = (x>>4) ^ (x>>1)&0xF ^ (x<<3)&0x8;
	    x = t[n][2][a] | t[n][3][b] << 4;
	    qbox[n][i] = x;
	}
    }
}
