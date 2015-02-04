; -------------------------------------------------------
; Files:
;
; 2fish_avr.cfg
;	Compile-time options/info (incl. key size)
; 2fish_avr.s
;	Twofish encryption algorithm
; 2fish_c_avr.h
; 2fish_c_avr.s
; 	Wrapper for interfacing with C programs
; avrmacros.s
;	Useful GNU as macros for the AVR
; example.s
;	Test/demo program
; kat_test.s
; 	Known Answer Tests in AVR assembly
; kat_example.c 
;	Example/Known Answer Tests in C
; driver.c
;	Simple AVR emulator using simavr
;
; -------------------------------------------------------
; Overview of most interesting implementations
; Column information:
;
; SIZE	Actual footprint of code + tables (excl. alignment)
; ASIZE	Minimum amount of flash necessary to load (incl. alignment)
; SRAM	Total amount of SRAM needed (.bss and stack)
; E	Cycles needed to encrypt one block
; KE	Cycles needed to keyschedule & encrypt one block

; Implementations not pre-computing the key-dependent s-box:
; Per SRAM size:
;
; 1) Smallest possible
; 2) Trade-off favouring space
; 3) Trade-off favouring speed
; 4) Fastest possible

SIZE: 724	ASIZE: 768	SRAM: 42	E: 83511 KE: 83891	; INLINE_round_g=0 UNROLL_round_h=0 UNROLL_round_g=0 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=0 TAB_key=0 TAB_sbox=0 TAB_q=0 SRAM_q=1 UNROLL_whiten=1 INLINE_whiten=0
SIZE: 1130	ASIZE: 1536	SRAM: 37	E: 17448 KE: 17828	; INLINE_round_g=0 UNROLL_round_h=0 UNROLL_round_g=1 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=1 TAB_key=0 TAB_sbox=0 TAB_q=1 SRAM_q=0 UNROLL_whiten=1 INLINE_whiten=0
SIZE: 1498	ASIZE: 1536	SRAM: 35	E: 12624 KE: 13004	; INLINE_round_g=1 UNROLL_round_h=1 UNROLL_round_g=1 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=1 TAB_key=0 TAB_sbox=0 TAB_q=1 SRAM_q=0 UNROLL_whiten=1 INLINE_whiten=0
SIZE: 2274	ASIZE: 2560	SRAM: 35	E: 11939 KE: 12319	; INLINE_round_g=1 UNROLL_round_h=1 UNROLL_round_g=1 UNROLL_keypair=1 UNROLL_enc=1 UNROLL_swap=1 TAB_key=0 TAB_sbox=0 TAB_q=1 SRAM_q=0 UNROLL_whiten=1 INLINE_whiten=1

SIZE: 688	ASIZE: 704	SRAM: 179	E: 38006 KE: 83629	; INLINE_round_g=0 UNROLL_round_h=0 UNROLL_round_g=0 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=0 TAB_key=1 TAB_sbox=0 TAB_q=0 SRAM_q=1 UNROLL_whiten=0 INLINE_whiten=0
SIZE: 1136	ASIZE: 1536	SRAM: 174	E: 7769 KE: 17332	; INLINE_round_g=0 UNROLL_round_h=0 UNROLL_round_g=1 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=1 TAB_key=1 TAB_sbox=0 TAB_q=1 SRAM_q=0 UNROLL_whiten=1 INLINE_whiten=0
SIZE: 1524	ASIZE: 1536	SRAM: 174	E: 5785 KE: 12929	; INLINE_round_g=1 UNROLL_round_h=1 UNROLL_round_g=1 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=1 TAB_key=1 TAB_sbox=0 TAB_q=1 SRAM_q=0 UNROLL_whiten=1 INLINE_whiten=0
SIZE: 2304	ASIZE: 2560	SRAM: 172	E: 5557 KE: 12321	; INLINE_round_g=1 UNROLL_round_h=1 UNROLL_round_g=1 UNROLL_keypair=1 UNROLL_enc=1 UNROLL_swap=1 TAB_key=1 TAB_sbox=0 TAB_q=1 SRAM_q=0 UNROLL_whiten=1 INLINE_whiten=1

SIZE: 684	ASIZE: 704	SRAM: 691	E: 11894 KE: 24877	; INLINE_round_g=0 UNROLL_round_h=0 UNROLL_round_g=0 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=0 TAB_key=1 TAB_sbox=0 TAB_q=1 SRAM_q=1 UNROLL_whiten=0 INLINE_whiten=0
SIZE: 762	ASIZE: 768	SRAM: 686	E: 7385 KE: 16468	; INLINE_round_g=0 UNROLL_round_h=0 UNROLL_round_g=1 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=1 TAB_key=1 TAB_sbox=0 TAB_q=1 SRAM_q=1 UNROLL_whiten=1 INLINE_whiten=0
SIZE: 1150	ASIZE: 1152	SRAM: 686	E: 5401 KE: 12065	; INLINE_round_g=1 UNROLL_round_h=1 UNROLL_round_g=1 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=1 TAB_key=1 TAB_sbox=0 TAB_q=1 SRAM_q=1 UNROLL_whiten=1 INLINE_whiten=0
SIZE: 1930	ASIZE: 1984	SRAM: 684	E: 5173 KE: 11457	; INLINE_round_g=1 UNROLL_round_h=1 UNROLL_round_g=1 UNROLL_keypair=1 UNROLL_enc=1 UNROLL_swap=1 TAB_key=1 TAB_sbox=0 TAB_q=1 SRAM_q=1 UNROLL_whiten=1 INLINE_whiten=1

; Implementations using key-dependent S-Boxes (requires >1K SRAM)
; Per SRAM size:
;
; 1) Trade-off favouring space
; 2) Trade-off favouring speed
; 3) Fastest possible
;
; NOTE:
; - The 'smallest possible implementations' in this category perform worse but have larger Flash/SRAM footprints than some S-Box-less configurations
; - These implementations offer a SRAM vs. space/key setup time trade-off

SIZE: 948	ASIZE: 960	SRAM: 1199	E: 4537 KE: 288237	; INLINE_round_g=0 UNROLL_round_h=0 UNROLL_round_g=1 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=1 TAB_key=1 TAB_sbox=1 TAB_q=0 SRAM_q=1 UNROLL_whiten=1 INLINE_whiten=0
SIZE: 1318	ASIZE: 1536	SRAM: 1198	E: 4537 KE: 50253	; INLINE_round_g=0 UNROLL_round_h=0 UNROLL_round_g=1 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=1 TAB_key=1 TAB_sbox=1 TAB_q=1 SRAM_q=0 UNROLL_whiten=1 INLINE_whiten=0
SIZE: 1514	ASIZE: 1536	SRAM: 1198	E: 3801 KE: 35779	; INLINE_round_g=1 UNROLL_round_h=1 UNROLL_round_g=1 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=1 TAB_key=1 TAB_sbox=1 TAB_q=1 SRAM_q=0 UNROLL_whiten=1 INLINE_whiten=0
SIZE: 2158	ASIZE: 2560	SRAM: 1196	E: 3573 KE: 35171	; INLINE_round_g=1 UNROLL_round_h=1 UNROLL_round_g=1 UNROLL_keypair=1 UNROLL_enc=1 UNROLL_swap=1 TAB_key=1 TAB_sbox=1 TAB_q=1 SRAM_q=0 UNROLL_whiten=1 INLINE_whiten=1

SIZE: 944	ASIZE: 960	SRAM: 1710	E: 4537 KE: 46701	; INLINE_round_g=0 UNROLL_round_h=0 UNROLL_round_g=1 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=1 TAB_key=1 TAB_sbox=1 TAB_q=1 SRAM_q=1 UNROLL_whiten=1 INLINE_whiten=0
SIZE: 1140	ASIZE: 1152	SRAM: 1710	E: 3801 KE: 32227	; INLINE_round_g=1 UNROLL_round_h=1 UNROLL_round_g=1 UNROLL_keypair=0 UNROLL_enc=0 UNROLL_swap=1 TAB_key=1 TAB_sbox=1 TAB_q=1 SRAM_q=1 UNROLL_whiten=1 INLINE_whiten=0
SIZE: 1784	ASIZE: 1792	SRAM: 1708	E: 3573 KE: 31619	; INLINE_round_g=1 UNROLL_round_h=1 UNROLL_round_g=1 UNROLL_keypair=1 UNROLL_enc=1 UNROLL_swap=1 TAB_key=1 TAB_sbox=1 TAB_q=1 SRAM_q=1 UNROLL_whiten=1 INLINE_whiten=1


; In general:
; UNROLL_round_g = UNROLL_swap = UNROLL_whiten = 1, INLINE_whiten=0
; 	Other settings result in unfavourable trade-offs.
; UNROLL_keypair = UNROLL_enc = 0
; 	These control 'outer loops' which do not need to be unrolled.
; INLINE_round_g = UNROLL_round_h

