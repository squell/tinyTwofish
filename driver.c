/*
	i2ctest.c

	Copyright 2008-2011 Michel Pollet <buserror@gmail.com>

 	This file is part of simavr.

	simavr is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	simavr is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with simavr.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <stdio.h>
#include <libgen.h>
#include <pthread.h>

#include "sim_avr.h"
#include "sim_core.h"
#include "avr_twi.h"
#include "sim_elf.h"
#include "sim_gdb.h"
#include "sim_vcd_file.h"

avr_t * avr = NULL;
avr_vcd_t vcd_file;

void dump_avr(void) 
{
	int dst;
	READ_SREG_INTO(avr, dst);
	printf("%10llu: ", (unsigned long long int)avr->cycle);
	for(int j=0; j < 1; j++) {
		for(int i=0; i < 32; i++)
			printf("%02x ", avr->data[i+j*32]);
		printf("SP=%04x, SREG=%02x, PC=%04lx [%04x]\n", avr->data[0x5E]*256+avr->data[0x5D], dst, (unsigned long)avr->pc/2, avr->flash[avr->pc]+avr->flash[avr->pc+1]*256);
	}
}

void putty(struct avr_t *avr, avr_io_addr_t addr, uint8_t v, void* param)
{
	if(v==4) {
		dump_avr();
		exit(0);
	}
	putchar(v);
	fflush(stdout);
}

#define foo(s) #s
#define str(s) foo(s)
#ifndef MCU
#define MCU attiny45
#endif

int main(int argc, char *argv[])
{
	elf_firmware_t f;
	const char * fname = argv[1]?argv[1]:"firmware.axf";
	const char * mcu   = argv[1]&&argv[2]?argv[2]:str(MCU);
	int debug = argc > 3;

	printf("Firmware pathname is %s\n", fname);
	elf_read_firmware(fname, &f);

	printf("firmware %s f=%d mmcu=%s\n", fname, (int)f.frequency, f.mmcu);

	avr = avr_make_mcu_by_name(mcu);
	if (!avr) {
		fprintf(stderr, "%s: AVR '%s' not known\n", argv[0], f.mmcu);
		exit(1);
	}
	avr_init(avr);
	avr_load_firmware(avr, &f);

	// even if not setup at startup, activate gdb if crashing
	avr->gdb_port = 1234;
	if (debug) {
		avr->state = cpu_Stopped;
		avr_gdb_init(avr);
	}

	printf( "\nDemo launching:\n");

	avr_register_io_write(avr,0xC6,putty,0);

	int state = cpu_Running;
	while ((state != cpu_Done) && (state != cpu_Crashed)) {
#ifndef FAST
		dump_avr();
#endif
		state = avr_run(avr);
	}
	dump_avr();
}
