.text
main:
	addi $a0, $zero, 100
	addi $a1, $zero, 200
	li	 $t9, add
	jalr $t9
	nop
	j	 done
	nop

add:
	add $v0, $a0, $a1
	jr $ra
	nop

sub:
	sub $v0, $a0, $a0
	jr $ra
	nop

done:
	.word 0xdeadbeef
