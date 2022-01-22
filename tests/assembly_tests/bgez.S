.text

addiu $t0, $zero, 1
addiu $t1, $zero, 10 

loop:
	addu  $t0, $t0, $t0
	addi  $t1, $t1, -1
	bgez  $t1, loop
	nop
	.word 0xdeadbeef
