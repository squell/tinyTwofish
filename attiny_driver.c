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
#include "avr_twi.h"
#include "sim_elf.h"
#include "sim_gdb.h"
#include "sim_vcd_file.h"

avr_t * avr = NULL;
avr_vcd_t vcd_file;

void dump_avr(void) 
{
    printf("%10lld: ", avr->cycle);
    for(int j=0; j < 1; j++) {
	for(int i=0; i < 32; i++)
	    printf("%02x ", avr->data[i+j*32]);
	printf("\n");
    }
}

int main(int argc, char *argv[])
{
	elf_firmware_t f;
	const char * fname = argv[1]?argv[1]:"firmware.axf";

	printf("Firmware pathname is %s\n", fname);
	elf_read_firmware(fname, &f);

	printf("firmware %s f=%d mmcu=%s\n", fname, (int)f.frequency, f.mmcu);

	avr = avr_make_mcu_by_name("attiny85");
	if (!avr) {
		fprintf(stderr, "%s: AVR '%s' not known\n", argv[0], f.mmcu);
		exit(1);
	}
	avr_init(avr);
	avr_load_firmware(avr, &f);

	// even if not setup at startup, activate gdb if crashing
	avr->gdb_port = 1234;
	if (0) {
		avr->state = cpu_Stopped;
		avr_gdb_init(avr);
	}

	printf( "\nDemo launching:\n");

	int state = cpu_Running;
	dump_avr();
	while ((state != cpu_Done) && (state != cpu_Crashed)) {
		state = avr_run(avr);
		dump_avr();
	}
}
