#! /bin/bash

OPTS="INLINE_round_g UNROLL_round_h UNROLL_round_g UNROLL_keypair UNROLL_enc UNROLL_swap TAB_key TAB_sbox TAB_q SRAM_q UNROLL_whiten INLINE_whiten"

MCU=atmega644
AS="avr-as -o2fish_avr.o -mmcu=$MCU 2> /dev/null"

test_script() {
    cp makefile *.s /tmp
    for flag in $OPTS; do
	echo "for $flag in 0 1; do"
    done
    echo -n "sed \""
    for flag in $OPTS; do
	echo -n "s/^$flag[[:space:]]*=.*/$flag=\$$flag/;"
    done
    echo "s/^KEY_SIZE[[:space:]]*=.*/KEY_SIZE=$1/\" 2fish_avr.cfg > /tmp/2fish_avr.cfg"
    cat <<script
	if make -B -s -C /tmp
	then
	    clocks0=\`./driver /tmp/rom0 atmega644 | tail -n1 | cut -f1 -d:\`
	    clocks1=\`./driver /tmp/rom1 atmega644 | tail -n1 | cut -f1 -d:\`
	    clocks2=\`./driver /tmp/rom2 atmega644 | tail -n1 | cut -f1 -d:\`
	    clocks3=\`./driver /tmp/rom3 atmega644 | tail -n1 | cut -f1 -d:\`
	    output4=\`./driver /tmp/rom4 atmega644 | tail -n1\`
	    clocks4=\`echo "\$output4" | cut -f1 -d:\`
	    sp=\`echo "\$output4" | cut -f2 -d=\`
	    sp=\$((4096+255-sp))
	    size=\`avr-nm --size-sort --radix=d /tmp/2fish_avr.o\`
	    rtot=\`echo "\$size" | awk '\$2~/^[tT]\$/{acc+=\$1}END{print acc}'\`
	    tot=\`avr-size --radix=10 --common /tmp/2fish_avr.o | tail -n1 | cut -f1,3\`
	    bss=\`echo "\$tot" | cut -f2\`
	    echo SIZE: \$rtot '\t' ASIZE: \$tot '\t' SRAM: \$((bss+sp)) '\t' INIT: \$((clocks0)) '\t' KS: \$((clocks2-clocks1)) '\t' EB: \$((clocks4-clocks3)) KE: \$((clocks2+clocks4-clocks1-clocks3)) '\t' '\t' SP: \$sp '\t' \#: \$size '\t' KEY_SIZE=$1 $(for flag in $OPTS; do echo -n " $flag=\$$flag"; done)
	else
	    echo 1>&2 not run
	fi
script
    for flag in $OPTS; do
	echo "done"
    done
}

make -s driver
test_script 128 "9f 58 9f 5c f6 12 2c 32 b6 bf ec 2f 2a e8 c3 5a" | sh
test_script 192 "cf d1 d2 e5 a9 be 9c df 50 1f 13 b8 92 bd 22 48" | sh
test_script 256 "37 52 7b e0 05 23 34 b8 9f 0c fc ca e8 7c fa 20" | sh
