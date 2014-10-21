OPTS="INLINE_round_g UNROLL_round_h UNROLL_round_g UNROLL_keypair UNROLL_enc UNROLL_swap TAB_key TAB_sbox TAB_q SRAM_q UNROLL_whiten INLINE_whiten"

MCU=atmega644
AS="avr-as -o2fish_avr.o -mmcu=$MCU 2> /dev/null"
ROM=megakat

test_script() {
    cp makefile *.s /tmp
    echo "echo 1>&2 {KEY_SIZE=$1}"
    for flag in $OPTS; do
	echo "for $flag in 0 1; do echo 1>&2 {$flag=\$$flag}"
    done
    echo -n "sed \""
    for flag in $OPTS; do
	echo -n "s/^$flag[[:space:]]*=.*/$flag=\$$flag/;"
    done
    echo "s/^KEY_SIZE[[:space:]]*=.*/KEY_SIZE=$1/\" 2fish_avr.cfg > /tmp/2fish_avr.cfg"
    cat <<script
	if make -B -s . -C /tmp $ROM
	then
	    ./driver /tmp/$ROM $MCU | tail -n1 | grep "$2" 1> /dev/null || echo failed: KEY_SIZE=$1 $(for flag in $OPTS; do echo -n " $flag=\$$flag"; done)
	else
	    echo 1>&2 not run
	fi
script
    for flag in $OPTS; do
	echo "done"
    done
}

#test_script 128 "9f 58 9f 5c f6 12 2c 32 b6 bf ec 2f 2a e8 c3 5a" | sh
#test_script 128 "df bd f3 99 d0 d3 11 2e d1 19 00 42 7b f1 f9 f4" | sh
#test_script 192 "cf d1 d2 e5 a9 be 9c df 50 1f 13 b8 92 bd 22 48" | sh
#test_script 256 "37 52 7b e0 05 23 34 b8 9f 0c fc ca e8 7c fa 20" | sh
test_script 128 "5d 9d 4e ef fa 91 51 57 55 24 f1 15 81 5a 12 e0" | sh
test_script 192 "e7 54 49 21 2b ee f9 f4 a3 90 bd 86 0a 64 09 41" | sh
test_script 256 "37 fe 26 ff 1c f6 61 75 f5 dd f4 c3 3b 97 a2 05" | sh
