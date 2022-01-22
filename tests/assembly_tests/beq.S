.text

li	$t0, 0
li	$t1, 10

beq $t0, $zero, done
nop
addiu	$t0, $t0, 4
nop

done:
.word 0xdeadbeef
nop
nop
nop
nop
nop
