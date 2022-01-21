.text
lh		$t1, d1
lh		$t2, d1 + 1
lh		$t3, d1 + 2
lh		$t4, d1 + 3
lh		$t5, d2
nop
nop
nop
nop
nop

.data
d1: .word 0xabcd0000
d2: .word 0xdeadbeef
