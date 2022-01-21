.text
lhu		$t1, d1
lhu		$t2, d1 + 1
lhu		$t3, d1 + 2
lhu		$t4, d1 + 3
lhu		$t5, d2
nop
nop
nop
nop
nop

.data
d1: .word 0xabcd0000
d2: .word 0xdeadbeef
