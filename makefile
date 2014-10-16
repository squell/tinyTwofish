CHIP = attiny45
SIMAVR_ROOT = /tmp/avr
SIMAVR_INCLUDE = ${SIMAVR_ROOT}/include/simavr
SIMAVR_LIB = ${SIMAVR_ROOT}/lib

CFLAGS = -O3

tinyrom: 2fish_avr.o
	avr-ld $< -o $@

megarom: 2fish_avr.o
	avr-ld $< -Tdata 0x800100 -o $@

.s.o:
	avr-as -mmcu ${CHIP} -o $@ $<

driver: driver.c
	c99 ${CFLAGS} $< -I ${SIMAVR_INCLUDE} -L ${SIMAVR_LIB} -lsimavr -lelf -o $@

selftest:
	! ./exhaustive_test.sh 2> /dev/null | grep -m 1 ''
