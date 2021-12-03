.text
lw	$t1, 0($sp)
lw  $t2, 4($t0)
lw	$t3, 8($t0)
lw  $t0, tdata0
lw  $t1, 0($t0)

.data
tdata0:
	.word 0xabcdabcd
tdata1:
	.word 0x11110000
tdata2:
	.word 0x11001010
