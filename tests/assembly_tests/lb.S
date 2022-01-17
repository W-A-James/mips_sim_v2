.text
li		$t0, 0x0040
lb		$t1, d1
lb		$t2, d1 + 1
lb		$t3, d1 + 2
lb		$t4, d1 + 3
lb		$t5, d1 + 4

.data
d1: .word 0xabcd0000
d2: .word 0xdeadbeef
