.text
li		$t0, 0xaaaabbbb
sw		$t0, d1
lw		$t1, d1
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
