.text
.globl
main:
	addi $a0, $zero, 100
	addi $a1, $zero, 200
	jal  add
	nop
	j	 done
	nop

.globl
add:
	add $v0, $a0, $a1
	jr $ra
	nop

.globl
sub:
	sub $v0, $a0, $a0
	jr $ra
	nop

done:
	.word 0xdeadbeef
