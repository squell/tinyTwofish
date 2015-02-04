CHIP ?= attiny85

SIMAVR_ROOT = /tmp/avr
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

tinyrom: example.o 2fish_avr.o
	avr-ld example.o 2fish_avr.o -o $@

megarom: example.o 2fish_avr.o
	avr-ld example.o 2fish_avr.o -Tdata 0x800100 -o $@

tinykat: kat_test.o 2fish_avr.o
	avr-ld kat_test.o 2fish_avr.o -o $@

megakat: kat_test.o 2fish_avr.o
	avr-ld kat_test.o 2fish_avr.o -Tdata 0x800100 -o $@

ckat: kat_example.o 2fish_avr.o 2fish_c_avr.o
	avr-gcc ${CFLAGS} -mmcu=${CHIP} kat_example.o 2fish_c_avr.o 2fish_avr.o -o $@

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

.s.o:
	avr-as -mmcu ${CHIP} -o $@ $<
.c.o:
	avr-gcc ${CFLAGS} -mmcu=${CHIP} -c -o $@ $<

driver: driver.c
	c99 ${CFLAGS} $< -DMCU=${CHIP} -I ${SIMAVR_INCLUDE} -L ${SIMAVR_LIB} -lsimavr -lelf -o $@

test: driver
	./selftest.sh 2>&1 | sed /failed/q1
	@echo All tests OK.
