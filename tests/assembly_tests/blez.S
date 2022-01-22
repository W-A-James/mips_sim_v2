.text

addi	$t0, $zero, -100
addiu	$t1, $zero, 0

loop:
	addi	$t1, $t1, 2
	addi	$t0, $t0, 1
	blez	$t0, loop
	nop

.word 0xdeadbeef

	
