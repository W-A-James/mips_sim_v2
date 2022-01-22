.text

addiu $t0, $zero, 100

loop:
	addi $t0, $t0, -1
	bgtz $t0, loop
	nop

done:
.word 0xdeadbeef

