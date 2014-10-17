CHIP ?= attiny45

SIMAVR_ROOT = /tmp/avr
SIMAVR_INCLUDE = ${SIMAVR_ROOT}/include/simavr
SIMAVR_LIB = ${SIMAVR_ROOT}/lib

CFLAGS = -O3

tinyrom: example.o 2fish_avr.o
	avr-ld example.o 2fish_avr.o -o $@

megarom: example.o 2fish_avr.o
	avr-ld example.o 2fish_avr.o -Tdata 0x800100 -o $@

tinykat: kat_test.o 2fish_avr.o
	avr-ld kat_test.o 2fish_avr.o -o $@

megakat: kat_test.o 2fish_avr.o
	avr-ld kat_test.o 2fish_avr.o -Tdata 0x800100 -o $@

2fish_avr.o: 2fish_avr.s 2fish_avr.cfg
example.o: example.s 2fish_avr.cfg
kat_test.o: kat_test.s 2fish_avr.cfg

.s.o:
	avr-as -mmcu ${CHIP} -o $@ $<

driver: driver.c
	c99 ${CFLAGS} $< -I ${SIMAVR_INCLUDE} -L ${SIMAVR_LIB} -lsimavr -lelf -o $@

test: driver
	./selftest.sh 2>&1 | sed /failed/q1
	@echo All tests OK.
