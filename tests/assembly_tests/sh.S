.text
ori		$t0, $zero, 0xabff 
sh		$t0, d1
lhu		$t1, d1
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
