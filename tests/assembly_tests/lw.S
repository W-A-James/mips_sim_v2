.text
lw	$t1, tdata0
lw  $t2, tdata1 
lw	$t3, tdata2 
nop
nop
nop
nop
nop
nop

.data
tdata0:
	.word 0xabcdabcd
tdata1:
	.word 0x11110000
tdata2:
	.word 0x11001010
