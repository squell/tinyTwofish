CHIP = attiny45

tinyrom: 2fish_avr.o
	avr-ld $< -o $@

megarom: 2fish_avr.o
	avr-ld $< -Tdata 0x800100 -o $@

.s.o:
	avr-as -mmcu ${CHIP} -o $@ $<

