.text

addi $t0, $zero, 5
move $t1, $zero
loop:
	add  $t1, $t1, $t0
	addi $t0, $t0, -1
	bne	 $t0, $zero, loop
	nop
