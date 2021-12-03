.text

addi $t0, $zero, 1
addi $t1, $zero, 29

loop:
	addu  $t0, $t0, $t0
	addiu $t1, $t1, -1
	bgez  $t1, loop
	nop
