.text
li		$t0, 0
lbu		$t1, d1
lbu		$t2, d1 + 1
lbu		$t3, d1 + 2
lbu		$t4, d1 + 3
lbu		$t5, d1 + 4 
nop
nop
nop
nop
nop
nop
nop

.data
d1: .word 0xabcd0000
d2: .word 0xdeadbeef
