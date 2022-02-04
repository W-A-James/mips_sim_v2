.text
main:
	addi $a0, $zero, 100
	addi $a1, $zero, 200
	la	 $t9, add
    nop
    nop
	jalr $t9
    nop
	nop
	j	 done
	nop

add:
	add $v0, $a0, $a1
    nop
	jr $ra
	nop

sub:
	sub $v0, $a0, $a0
    nop
	jr $ra
	nop

done:
	.word 0xdeadbeef
