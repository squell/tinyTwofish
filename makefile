CHIP ?= attiny85

SIMAVR_ROOT = /usr
SIMAVR_INCLUDE = ${SIMAVR_ROOT}/include/simavr
SIMAVR_LIB = ${SIMAVR_ROOT}/lib

CFLAGS = -O3

all: rom0 rom1 rom2 rom3 rom4

rom0: example0.o 2fish_avr.o
	avr-ld example0.o 2fish_avr.o -Tdata 0x800100 -o $@
rom1: example1.o 2fish_avr.o
	avr-ld example1.o 2fish_avr.o -Tdata 0x800100 -o $@
rom2: example2.o 2fish_avr.o
	avr-ld example2.o 2fish_avr.o -Tdata 0x800100 -o $@
rom3: example3.o 2fish_avr.o
	avr-ld example3.o 2fish_avr.o -Tdata 0x800100 -o $@
rom4: example4.o 2fish_avr.o
	avr-ld example4.o 2fish_avr.o -Tdata 0x800100 -o $@

clean: 
	rm -f *.o tinyrom megarom tinykat megakat ckat rom?

# avr-ld doesn't know about the MCU names, and the SRAM starts at
# different address for different chips
#
tinyrom: example.o 2fish_avr.o
	avr-ld example.o 2fish_avr.o -o $@
megarom: example.o 2fish_avr.o
	avr-ld example.o 2fish_avr.o -Tdata 0x800100 -o $@
hugerom: example.o 2fish_avr.o
	avr-ld example.o 2fish_avr.o -m avr6 -Tdata 0x800200 -o $@
tinykat: kat_test.o 2fish_avr.o
	avr-ld kat_test.o 2fish_avr.o -o $@
megakat: kat_test.o 2fish_avr.o
	avr-ld kat_test.o 2fish_avr.o -Tdata 0x800100 -o $@
hugekat: kat_test.o 2fish_avr.o
	avr-ld kat_test.o 2fish_avr.o -m avr6 -Tdata 0x800200 -o $@

# alternative to above: let avr-gcc figure it out for us
rom: example.o 2fish_avr.o
	avr-gcc -nostdlib -mmcu=${CHIP} $+ -o $@

kat: kat_test.o 2fish_avr.o
	avr-gcc -nostdlib -mmcu=${CHIP} $+ -o $@

ckat: kat_example.o 2fish_c_avr.o 2fish_avr.o
	avr-gcc ${CFLAGS} -mmcu=${CHIP} $+ -o $@

2fish_avr.o: 2fish_avr.s 2fish_avr.cfg

example0.o: example.s 2fish_avr.cfg
	avr-as --defsym STATE=0 -mmcu ${CHIP} -o $@ $<
example1.o: example.s 2fish_avr.cfg
	avr-as --defsym STATE=1 -mmcu ${CHIP} -o $@ $<
example2.o: example.s 2fish_avr.cfg
	avr-as --defsym STATE=2 -mmcu ${CHIP} -o $@ $<
example3.o: example.s 2fish_avr.cfg
	avr-as --defsym STATE=3 -mmcu ${CHIP} -o $@ $<
example4.o: example.s 2fish_avr.cfg
	avr-as --defsym STATE=4 -mmcu ${CHIP} -o $@ $<

%.o: %.s
	avr-as -mmcu ${CHIP} -o $@ $<
%.o: %.c
	avr-gcc ${CFLAGS} -mmcu=${CHIP} -c -o $@ $<

%.hex: %
	avr-objcopy -j .text -j .data -O ihex $< $@

driver: driver.c
	c99 ${CFLAGS} $< -DMCU=${CHIP} -I ${SIMAVR_INCLUDE} -L ${SIMAVR_LIB} -lsimavr -lelf -o $@

test: driver
	./selftest.sh 2>&1 | sed /failed/q1
	@echo All tests OK.
