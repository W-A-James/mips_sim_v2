.text

li	$t0, 0
li	$t1, 10

loop:

addiu	$t0, $t0, 4
addi	$t1, $t1, -1
bne		$t1, $zero, loop
nop

.word 0xdeadbeef
nop
nop
nop
nop
nop
