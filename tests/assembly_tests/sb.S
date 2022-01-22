.text
addiu   $t0, $zero, 64
sb		$t0, d1
lbu		$t1, d1
.word	0xdeadbeef
nop
nop
nop
nop
nop
nop

.data
d1: .word 0xabcd0000
d2: .word 0xdeadbeef
