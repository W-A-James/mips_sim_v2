.text

move	$t0, $zero
addi	$t1, $zero, 1
la		$t2, done
jr		$t2
add		$t1, $t1, $t1
add		$t0, $t1, $t1

done:
    .word 0xdeadbeef
	nop
