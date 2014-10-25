/*

 Reproduced the Test vectors from the Twofish paper (avr-gcc adaptation)

 Puts an AVR to sleep if succesful; endless loop if unsuccesful.

 copyright (c) 2014 marc schoolderman

 permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is
 hereby granted, provided that the above copyright notice and this permission notice appear in all copies.

 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "2fish_c_avr.h"

/* The CT=... lines from KAT.txt */

unsigned char KAT_answers[3][49][16] = {
    0x9F,0x58,0x9F,0x5C,0xF6,0x12,0x2C,0x32,0xB6,0xBF,0xEC,0x2F,0x2A,0xE8,0xC3,0x5A,
    0xD4,0x91,0xDB,0x16,0xE7,0xB1,0xC3,0x9E,0x86,0xCB,0x08,0x6B,0x78,0x9F,0x54,0x19,
    0x01,0x9F,0x98,0x09,0xDE,0x17,0x11,0x85,0x8F,0xAA,0xC3,0xA3,0xBA,0x20,0xFB,0xC3,
    0x63,0x63,0x97,0x7D,0xE8,0x39,0x48,0x62,0x97,0xE6,0x61,0xC6,0xC9,0xD6,0x68,0xEB,
    0x81,0x6D,0x5B,0xD0,0xFA,0xE3,0x53,0x42,0xBF,0x2A,0x74,0x12,0xC2,0x46,0xF7,0x52,
    0x54,0x49,0xEC,0xA0,0x08,0xFF,0x59,0x21,0x15,0x5F,0x59,0x8A,0xF4,0xCE,0xD4,0xD0,
    0x66,0x00,0x52,0x2E,0x97,0xAE,0xB3,0x09,0x4E,0xD5,0xF9,0x2A,0xFC,0xBC,0xDD,0x10,
    0x34,0xC8,0xA5,0xFB,0x2D,0x3D,0x08,0xA1,0x70,0xD1,0x20,0xAC,0x6D,0x26,0xDB,0xFA,
    0x28,0x53,0x0B,0x35,0x8C,0x1B,0x42,0xEF,0x27,0x7D,0xE6,0xD4,0x40,0x7F,0xC5,0x91,
    0x8A,0x8A,0xB9,0x83,0x31,0x0E,0xD7,0x8C,0x8C,0x0E,0xCD,0xE0,0x30,0xB8,0xDC,0xA4,
    0x48,0xC7,0x58,0xA6,0xDF,0xC1,0xDD,0x8B,0x25,0x9F,0xA1,0x65,0xE1,0xCE,0x2B,0x3C,
    0xCE,0x73,0xC6,0x5C,0x10,0x16,0x80,0xBB,0xC2,0x51,0xC5,0xC1,0x6A,0xBC,0xF2,0x14,
    0xC7,0xAB,0xD7,0x4A,0xA0,0x60,0xF7,0x8B,0x24,0x4E,0x24,0xC7,0x13,0x42,0xBA,0x89,
    0xD0,0xF8,0xB3,0xB6,0x40,0x9E,0xBC,0xB6,0x66,0xD2,0x9C,0x91,0x65,0x65,0xAB,0xFC,
    0xDD,0x42,0x66,0x29,0x08,0x07,0x00,0x54,0x54,0x4F,0xE0,0x9D,0xA4,0x26,0x31,0x30,
    0x70,0x07,0xBA,0xCB,0x42,0xF7,0xBF,0x98,0x9C,0xF3,0x0F,0x78,0xBC,0x50,0xED,0xCA,
    0x57,0xB9,0xA1,0x8E,0xE9,0x7D,0x90,0xF4,0x35,0xA1,0x6F,0x69,0xF0,0xAC,0x6F,0x16,
    0x06,0x18,0x1F,0x0D,0x53,0x26,0x7A,0xBD,0x8F,0x3B,0xB2,0x84,0x55,0xB1,0x98,0xAD,
    0x81,0xA1,0x2D,0x84,0x49,0xE9,0x04,0x0B,0xAA,0xE7,0x19,0x63,0x38,0xD8,0xC8,0xF2,
    0xBE,0x42,0x26,0x51,0xC5,0x6F,0x26,0x22,0xDA,0x02,0x01,0x81,0x5A,0x95,0xA8,0x20,
    0x11,0x3B,0x19,0xF2,0xD7,0x78,0x47,0x39,0x90,0x48,0x0C,0xEE,0x4D,0xA2,0x38,0xD1,
    0xE6,0x94,0x2E,0x9A,0x86,0xE5,0x44,0xCF,0x3E,0x33,0x64,0xF2,0x0B,0xE0,0x11,0xDF,
    0x87,0xCD,0xC6,0xAA,0x48,0x7B,0xFD,0x0E,0xA7,0x01,0x88,0x25,0x7D,0x9B,0x38,0x59,
    0xD5,0xE2,0x70,0x12,0x53,0xDD,0x75,0xA1,0x1A,0x4C,0xFB,0x24,0x37,0x14,0xBD,0x14,
    0xFD,0x24,0x81,0x2E,0xEA,0x10,0x7A,0x9E,0x6F,0xAB,0x8E,0xAB,0xE0,0xF0,0xF4,0x8C,
    0xDA,0xFA,0x84,0xE3,0x1A,0x29,0x7F,0x37,0x2C,0x3A,0x80,0x71,0x00,0xCD,0x78,0x3D,
    0xA5,0x5E,0xD2,0xD9,0x55,0xEC,0x89,0x50,0xFC,0x0C,0xC9,0x3B,0x76,0xAC,0xBF,0x91,
    0x2A,0xBE,0xA2,0xA4,0xBF,0x27,0xAB,0xDC,0x6B,0x6F,0x27,0x89,0x93,0x26,0x47,0x44,
    0x04,0x53,0x83,0xE2,0x19,0x32,0x1D,0x5A,0x44,0x35,0xC0,0xE4,0x91,0xE7,0xDE,0x10,
    0x74,0x60,0xA4,0xCD,0x4F,0x31,0x2F,0x32,0xB1,0xC7,0xA9,0x4F,0xA0,0x04,0xE9,0x34,
    0x6B,0xBF,0x91,0x86,0xD3,0x2C,0x2C,0x58,0x95,0x64,0x9D,0x74,0x65,0x66,0x05,0x0A,
    0xCD,0xBD,0xD1,0x9A,0xCF,0x40,0xB8,0xAC,0x03,0x28,0xC8,0x00,0x54,0x26,0x60,0x68,
    0x1D,0x28,0x36,0xCA,0xE4,0x22,0x3E,0xAB,0x50,0x66,0x86,0x7A,0x71,0xB1,0xA1,0xC3,
    0x2D,0x7F,0x37,0x12,0x1D,0x0D,0x24,0x16,0xD5,0xE2,0x76,0x7F,0xF2,0x02,0x06,0x1B,
    0xD7,0x07,0x36,0xD1,0xAB,0xC7,0x42,0x7A,0x12,0x1C,0xC8,0x16,0xCD,0x66,0xD7,0xFF,
    0xAC,0x6C,0xA7,0x1C,0xBC,0xBE,0xDC,0xC0,0xEA,0x84,0x9F,0xB2,0xE9,0x37,0x78,0x65,
    0x30,0x72,0x65,0xFF,0x14,0x5C,0xBB,0xC7,0x10,0x4B,0x3E,0x51,0xC6,0xC1,0xD6,0xB4,
    0x93,0x4B,0x7D,0xB4,0xB3,0x54,0x48,0x54,0xDB,0xCA,0x81,0xC4,0xC5,0xDE,0x4E,0xB1,
    0x18,0x75,0x98,0x24,0xAD,0x98,0x23,0xD5,0x96,0x1F,0x84,0x37,0x7D,0x7E,0xAE,0xBF,
    0xDE,0xDD,0xAC,0x60,0x29,0xB0,0x15,0x74,0xD9,0xBA,0xBB,0x09,0x9D,0xC6,0xCA,0x6C,
    0x5E,0xA8,0x2E,0xEA,0x22,0x44,0xDE,0xD4,0x2C,0xCA,0x2F,0x83,0x5D,0x56,0x15,0xDF,
    0x1E,0x38,0x53,0xF7,0xFF,0xA5,0x70,0x91,0x77,0x1D,0xD8,0xCD,0xEE,0x94,0x14,0xDE,
    0x5C,0x2E,0xBB,0xF7,0x5D,0x31,0xF3,0x0B,0x5E,0xA2,0x6E,0xAC,0x87,0x82,0xD8,0xD1,
    0x3A,0x3C,0xFA,0x1F,0x13,0xA1,0x36,0xC9,0x4D,0x76,0xE5,0xFA,0x4A,0x11,0x09,0xFF,
    0x91,0x63,0x0C,0xF9,0x60,0x03,0xB8,0x03,0x2E,0x69,0x57,0x97,0xE3,0x13,0xA5,0x53,
    0x13,0x7A,0x24,0xCA,0x47,0xCD,0x12,0xBE,0x81,0x8D,0xF4,0xD2,0xF4,0x35,0x59,0x60,
    0xBC,0xA7,0x24,0xA5,0x45,0x33,0xC6,0x98,0x7E,0x14,0xAA,0x82,0x79,0x52,0xF9,0x21,
    0x6B,0x45,0x92,0x86,0xF3,0xFF,0xD2,0x8D,0x49,0xF1,0x5B,0x15,0x81,0xB0,0x8E,0x42,
    0x5D,0x9D,0x4E,0xEF,0xFA,0x91,0x51,0x57,0x55,0x24,0xF1,0x15,0x81,0x5A,0x12,0xE0,

    0xEF,0xA7,0x1F,0x78,0x89,0x65,0xBD,0x44,0x53,0xF8,0x60,0x17,0x8F,0xC1,0x91,0x01,
    0x88,0xB2,0xB2,0x70,0x6B,0x10,0x5E,0x36,0xB4,0x46,0xBB,0x6D,0x73,0x1A,0x1E,0x88,
    0x39,0xDA,0x69,0xD6,0xBA,0x49,0x97,0xD5,0x85,0xB6,0xDC,0x07,0x3C,0xA3,0x41,0xB2,
    0x18,0x2B,0x02,0xD8,0x14,0x97,0xEA,0x45,0xF9,0xDA,0xAC,0xDC,0x29,0x19,0x3A,0x65,
    0x7A,0xFF,0x7A,0x70,0xCA,0x2F,0xF2,0x8A,0xC3,0x1D,0xD8,0xAE,0x5D,0xAA,0xAB,0x63,
    0xD1,0x07,0x9B,0x78,0x9F,0x66,0x66,0x49,0xB6,0xBD,0x7D,0x16,0x29,0xF1,0xF7,0x7E,
    0x3A,0xF6,0xF7,0xCE,0x5B,0xD3,0x5E,0xF1,0x8B,0xEC,0x6F,0xA7,0x87,0xAB,0x50,0x6B,
    0xAE,0x81,0x09,0xBF,0xDA,0x85,0xC1,0xF2,0xC5,0x03,0x8B,0x34,0xED,0x69,0x1B,0xFF,
    0x89,0x3F,0xD6,0x7B,0x98,0xC5,0x50,0x07,0x35,0x71,0xBD,0x63,0x12,0x63,0xFC,0x78,
    0x16,0x43,0x4F,0xC9,0xC8,0x84,0x1A,0x63,0xD5,0x87,0x00,0xB5,0x57,0x8E,0x8F,0x67,
    0x95,0x94,0xCF,0x62,0xD4,0x8A,0xCD,0x34,0x7A,0x68,0xA3,0x16,0x1F,0x0F,0x3E,0xE7,
    0xB2,0x3E,0x8C,0x2C,0x73,0x1C,0x51,0x40,0x17,0xD1,0xF2,0xB8,0x8D,0x77,0xD2,0x08,
    0x93,0xCC,0x59,0x2B,0xC9,0x6D,0x95,0xFA,0x8A,0xC3,0x2D,0xA8,0x94,0xF6,0xAB,0x89,
    0x26,0x51,0x6E,0x6B,0xD4,0xAE,0xF8,0x6A,0xF4,0xF4,0xAD,0x58,0xFA,0x41,0xA1,0x4C,
    0x00,0xA8,0xFF,0xFA,0xB8,0x61,0x6B,0xE7,0x10,0xA6,0x59,0x24,0x38,0xFC,0x40,0xBE,
    0xA1,0x9B,0x81,0x1C,0x77,0x48,0x2D,0x97,0xC8,0x42,0xEC,0x62,0xDB,0x2E,0xDC,0xCE,
    0xD7,0xFF,0x43,0x86,0x78,0xD8,0x18,0xCA,0xA2,0x6A,0x67,0x63,0x42,0xF9,0x8E,0x8B,
    0x84,0xEA,0xFF,0xC0,0xC0,0x91,0x58,0x2A,0xBB,0x71,0x70,0x86,0xE0,0x80,0x7A,0x5F,
    0x55,0x00,0xAF,0x1C,0x79,0x50,0x3F,0xEF,0x1B,0xAC,0xF3,0x5A,0x81,0xDC,0x28,0x65,
    0x72,0x82,0xB2,0xF3,0xE7,0x66,0xC8,0x36,0x64,0x93,0x0A,0x19,0xD2,0x01,0xD7,0xE7,
    0x67,0x96,0x08,0x5C,0x32,0xFB,0xDD,0x2A,0xB4,0x3E,0x81,0xEA,0xC1,0x26,0x27,0x43,
    0x58,0x39,0xF9,0xE1,0x48,0xB9,0xFD,0x2B,0x5A,0x52,0x75,0x1D,0x4F,0x17,0x8F,0xDC,
    0xA8,0x8F,0x34,0x05,0x67,0x42,0xE5,0x54,0x08,0xA7,0xA9,0xE7,0xB6,0xD4,0xC8,0xC0,
    0x9C,0x8C,0x30,0x4C,0xB1,0xF9,0x37,0xC6,0xE4,0x25,0x28,0x45,0x9F,0xA8,0x87,0x2F,
    0xEA,0x36,0x68,0xC0,0xD9,0x65,0x29,0xA7,0xF3,0xBF,0x0F,0x7C,0x2B,0x5C,0x5B,0xE2,
    0xA8,0xFB,0x6E,0xEF,0xCA,0xAF,0x9C,0x40,0x41,0x07,0x2D,0x57,0x09,0x84,0xCE,0xD2,
    0xAB,0xF4,0x66,0x2E,0x5D,0x50,0xF7,0x1B,0x15,0xFE,0x3B,0x42,0x8A,0xFE,0x35,0x00,
    0x3B,0x3A,0xED,0x23,0x95,0x8D,0xA6,0xE2,0xFA,0x44,0x93,0xBC,0xBE,0x59,0xA8,0x06,
    0xCF,0xBF,0x44,0x6E,0x33,0xC3,0xDC,0xD4,0xDD,0x51,0x61,0xCA,0x00,0xD4,0xBA,0x8F,
    0x07,0x2A,0xDB,0xFA,0x7E,0xB9,0x62,0xBA,0x19,0x9A,0xFC,0x72,0x03,0x39,0xFF,0x29,
    0xAA,0xB3,0x46,0xD9,0x12,0x3A,0x81,0x40,0xAC,0x56,0x3E,0xF1,0x7E,0x70,0x54,0xC8,
    0x3C,0x7D,0xDC,0x5F,0xE6,0xE5,0x88,0x8F,0xE6,0x1D,0xED,0xA0,0xC6,0x9C,0xD3,0x20,
    0x34,0xAA,0x64,0x5C,0x7E,0x35,0x32,0x09,0x8A,0xDA,0x91,0xBB,0x12,0x8E,0xD8,0x21,
    0x7A,0x5D,0x12,0xE3,0x15,0x03,0x85,0x22,0xDA,0x01,0xEC,0x08,0x34,0xB1,0x32,0x2C,
    0x86,0xE2,0x35,0xA1,0xCB,0x09,0x1F,0xF7,0xFE,0x6F,0xBB,0xCA,0x0D,0x73,0xBE,0x58,
    0xF3,0xCE,0x81,0xCA,0xCE,0x5D,0x6B,0xA9,0xC5,0x58,0x11,0x1D,0xCD,0xB2,0x2F,0x5D,
    0xFB,0x30,0xA5,0xD6,0x7D,0x5F,0x5B,0xE4,0x04,0x8C,0x77,0xE2,0xAD,0x3B,0xC9,0xD1,
    0x31,0xE6,0x9D,0xD3,0xD2,0xD9,0x0E,0x81,0xC9,0xEB,0xFA,0xC2,0x57,0xE9,0x82,0x3D,
    0x87,0x57,0x9B,0x3F,0x19,0xA9,0xCD,0xE1,0x2B,0xB8,0x82,0xFF,0xEA,0xF6,0x65,0xAE,
    0x24,0xA8,0x57,0x2A,0xA8,0x44,0xFE,0xF2,0x5F,0x76,0x70,0xFA,0xE3,0x0F,0x1C,0xD2,
    0x0C,0xFA,0x32,0xE5,0xE3,0x3F,0x3B,0x2D,0xAC,0x9F,0x34,0xD2,0x59,0x79,0x31,0x9A,
    0x3C,0x64,0xD7,0xFC,0x88,0x1B,0x9B,0x82,0xAB,0xA2,0x1F,0xF1,0x22,0xB9,0x8F,0x54,
    0xA7,0x94,0xCA,0xEE,0x67,0x56,0x28,0x1B,0x7A,0x64,0x89,0x4E,0x4E,0x4F,0x70,0xA8,
    0x89,0xA9,0xBF,0x6B,0x89,0x3B,0xC5,0xE6,0xFE,0xF4,0xC7,0x7F,0x3D,0x0F,0x29,0xA6,
    0x5D,0xBE,0x44,0x03,0x27,0x69,0xDF,0x54,0x3E,0xAD,0x7A,0xD1,0x3A,0x5F,0x33,0x10,
    0xDE,0xA4,0xF3,0xDA,0x75,0xEC,0x7A,0x8E,0xAC,0x38,0x61,0xA9,0x91,0x24,0x02,0xCD,
    0xFB,0x66,0x52,0x2C,0x33,0x2F,0xCC,0x4C,0x04,0x2A,0xBE,0x32,0xFA,0x9E,0x90,0x2F,
    0xF0,0xAB,0x73,0x30,0x11,0x25,0xFA,0x21,0xEF,0x70,0xBE,0x53,0x85,0xFB,0x76,0xB6,
    0xE7,0x54,0x49,0x21,0x2B,0xEE,0xF9,0xF4,0xA3,0x90,0xBD,0x86,0x0A,0x64,0x09,0x41,

    0x57,0xFF,0x73,0x9D,0x4D,0xC9,0x2C,0x1B,0xD7,0xFC,0x01,0x70,0x0C,0xC8,0x21,0x6F,
    0xD4,0x3B,0xB7,0x55,0x6E,0xA3,0x2E,0x46,0xF2,0xA2,0x82,0xB7,0xD4,0x5B,0x4E,0x0D,
    0x90,0xAF,0xE9,0x1B,0xB2,0x88,0x54,0x4F,0x2C,0x32,0xDC,0x23,0x9B,0x26,0x35,0xE6,
    0x6C,0xB4,0x56,0x1C,0x40,0xBF,0x0A,0x97,0x05,0x93,0x1C,0xB6,0xD4,0x08,0xE7,0xFA,
    0x30,0x59,0xD6,0xD6,0x17,0x53,0xB9,0x58,0xD9,0x2F,0x47,0x81,0xC8,0x64,0x0E,0x58,
    0xE6,0x94,0x65,0x77,0x05,0x05,0xD7,0xF8,0x0E,0xF6,0x8C,0xA3,0x8A,0xB3,0xA3,0xD6,
    0x5A,0xB6,0x7A,0x5F,0x85,0x39,0xA4,0xA5,0xFD,0x9F,0x03,0x73,0xBA,0x46,0x34,0x66,
    0xDC,0x09,0x6B,0xCD,0x99,0xFC,0x72,0xF7,0x99,0x36,0xD4,0xC7,0x48,0xE7,0x5A,0xF7,
    0xC5,0xA3,0xE7,0xCE,0xE0,0xF1,0xB7,0x26,0x05,0x28,0xA6,0x8F,0xB4,0xEA,0x05,0xF2,
    0x43,0xD5,0xCE,0xC3,0x27,0xB2,0x4A,0xB9,0x0A,0xD3,0x4A,0x79,0xD0,0x46,0x91,0x51,
    0x05,0xBA,0x7B,0xE9,0xBE,0x52,0x74,0xFB,0xEB,0x4A,0xC5,0xFD,0x5F,0xAA,0xFA,0x10,
    0x89,0xD5,0x13,0xB9,0x89,0xE3,0xCE,0xCE,0x4D,0x2E,0x3E,0x4C,0x15,0xD4,0xE0,0x1C,
    0xE5,0x6E,0x46,0x2A,0xE2,0xE6,0x6F,0x80,0x0C,0xE2,0x22,0x4C,0x4C,0xAF,0xAC,0xBE,
    0xF5,0xDB,0xEE,0x54,0x14,0xD7,0x9C,0xA4,0x6B,0xE1,0x73,0x46,0xFD,0xC7,0x2B,0xF5,
    0xC7,0x80,0xFC,0x28,0x81,0x5E,0x66,0x7B,0x3D,0x22,0x4E,0xCF,0x1F,0x4A,0x7D,0x65,
    0x2E,0xF1,0x7A,0x0D,0x75,0x44,0x0E,0xCE,0xC9,0xAF,0x18,0xD2,0x9A,0xBA,0x3C,0xDA,
    0xD6,0x53,0xFD,0xFF,0x96,0xBC,0xF8,0x9A,0x92,0x9F,0x63,0x77,0xAB,0x41,0x96,0x7A,
    0x36,0xBB,0xAD,0x88,0x38,0x38,0x0C,0x87,0x93,0xA7,0xAE,0xA0,0xA1,0x1D,0x04,0xD2,
    0x03,0x4E,0xFE,0xCF,0xC0,0xEF,0xC0,0x0D,0x82,0xC3,0x34,0x5A,0x87,0x08,0xAE,0x78,
    0xEA,0x38,0x88,0x73,0x07,0x33,0x7A,0x29,0x34,0xA9,0xEB,0x80,0x2F,0x6C,0xFD,0xDD,
    0x7C,0x27,0x9D,0x47,0x77,0x5A,0xB4,0xC2,0x64,0x42,0xE8,0xA1,0x17,0x94,0x3D,0xD8,
    0xE3,0xCF,0x58,0x10,0x56,0xEB,0xC1,0x16,0x9C,0xF4,0x51,0xE9,0x30,0x30,0x87,0x26,
    0x65,0x52,0x7B,0x29,0xD4,0x89,0xC8,0xCD,0xDC,0x62,0xE5,0xE7,0xFF,0xC4,0xE9,0x24,
    0x3B,0x49,0x1F,0x93,0x02,0x30,0xA0,0x17,0x08,0x19,0xCA,0xCB,0x48,0xF9,0x03,0x0C,
    0xA7,0xC8,0x19,0x3F,0x35,0xAF,0x63,0xB5,0x1D,0x7F,0x9D,0xED,0xCE,0xC8,0x58,0x66,
    0x00,0x9A,0x48,0xF9,0xA1,0xC2,0x7F,0x9A,0xA5,0xF9,0xBD,0x90,0x9C,0x84,0x8B,0x60,
    0x3B,0x50,0x7E,0x98,0x7B,0x3F,0x82,0x7A,0x09,0x3B,0x26,0xC8,0x5C,0xDF,0xF6,0xC6,
    0xEA,0xFB,0x3B,0xA6,0xC7,0x8E,0x37,0x21,0x4F,0xE9,0x08,0x66,0x9B,0xC2,0x4F,0x6F,
    0x2B,0x09,0xF1,0x0D,0x7F,0x2B,0x62,0xA4,0xD0,0xDA,0xFF,0xB9,0xF8,0x82,0xB9,0x80,
    0xE6,0xB9,0xFE,0x90,0x70,0x09,0xB2,0xDC,0x64,0x94,0x12,0xDD,0x63,0x0A,0xE5,0x76,
    0xF2,0xE4,0xDC,0x89,0x72,0x4C,0xD5,0x8F,0x85,0x84,0x7B,0xD0,0x9E,0xD4,0x50,0x0F,
    0xDC,0x65,0x04,0x44,0xFC,0xEB,0xC2,0xBD,0x6E,0x45,0x0E,0xF4,0x60,0x5F,0xCA,0xBE,
    0xB7,0xCF,0xB3,0x17,0x55,0xFD,0xA2,0xAB,0x0A,0x67,0xFB,0xA0,0x99,0x01,0xA7,0x3E,
    0x9C,0x7E,0x75,0x78,0xF3,0xD2,0x92,0x3D,0xCD,0xAB,0xFE,0xD0,0xA5,0xEF,0x86,0xEE,
    0x94,0x65,0xE9,0xAE,0x17,0x64,0x83,0xBD,0x39,0x8C,0x80,0x33,0xAA,0x13,0x6F,0x68,
    0x85,0x51,0x27,0x9D,0xE9,0x99,0x18,0x05,0xFC,0xFC,0x19,0x37,0xC5,0x2A,0xE9,0xD4,
    0xA8,0x30,0xEB,0x8D,0xA5,0x2E,0xCF,0xF7,0xF9,0x3B,0x76,0x27,0x32,0x2C,0xF9,0xD7,
    0xEC,0x89,0xF0,0xE5,0x15,0x54,0x25,0xD4,0xB9,0x2B,0xD4,0xB2,0x00,0xC1,0xA6,0xE0,
    0x8A,0xF7,0x6A,0x78,0x20,0x61,0xD3,0x83,0x60,0xC6,0xC3,0xCD,0xDC,0xBE,0x15,0x16,
    0xD4,0xEE,0xAA,0x6A,0x06,0x96,0x59,0xD5,0xD8,0x85,0x90,0xDE,0x75,0x51,0x56,0x31,
    0x25,0x9C,0x02,0x1D,0x37,0xB0,0x77,0x19,0x7B,0x80,0xFC,0xDB,0x07,0xEA,0x1A,0xF2,
    0x30,0x0C,0xC8,0xB4,0x17,0x1F,0x0E,0x9B,0xD7,0x57,0x10,0xFA,0xD0,0x33,0xC5,0x70,
    0x5C,0xFA,0x5B,0xD2,0x13,0xA7,0x4F,0x02,0xE6,0x53,0x90,0xA4,0xC1,0x4A,0x1D,0xF6,
    0xA4,0x43,0xEA,0x1B,0x2C,0x57,0x47,0xCE,0x7E,0xC5,0xF2,0x1D,0x4F,0xE0,0xC1,0x47,
    0xD2,0xDE,0xD7,0x3E,0x59,0x31,0x9A,0x81,0x38,0xE0,0x33,0x1F,0x0E,0xA1,0x49,0xEA,
    0x2E,0x21,0x58,0xBC,0x3E,0x5F,0xC7,0x14,0xC1,0xEE,0xEC,0xA0,0xEA,0x69,0x6D,0x48,
    0x24,0x8A,0x7F,0x35,0x28,0xB1,0x68,0xAC,0xFD,0xD1,0x38,0x6E,0x3F,0x51,0xE3,0x0C,
    0x43,0x10,0x58,0xF4,0xDB,0xC7,0xF7,0x34,0xDA,0x4F,0x02,0xF0,0x4C,0xC4,0xF4,0x59,
    0x37,0xFE,0x26,0xFF,0x1C,0xF6,0x61,0x75,0xF5,0xDD,0xF4,0xC3,0x3B,0x97,0xA2,0x05,
};

int main(void)
{
    unsigned char buf[2][32];
    int i;

    memset(buf, 0, sizeof buf);
    for(i=1; i<=49; i++) {
	twofish_setkey(buf[i%2]);
	memcpy(buf[i%2]+16, buf[(i+1)%2], 16);
	memcpy(buf[i%2],    buf[(i+1)%2], 16);

	twofish_encrypt(buf[i%2]);
	if(memcmp(buf[i%2], KAT_answers[twofish_keysize/8-2][i-1], 16) != 0)
	    abort();
    }
    asm("cli");
    asm("sleep");
}

