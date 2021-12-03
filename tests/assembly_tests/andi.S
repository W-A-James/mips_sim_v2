.text

addiu	$t0, $zero, 0x0001
nop
nop
nop
nop
nop
sll		$t1, $t0, 31
andi	$t2, $zero, 0
andi	$t3, $zero, 1
andi	$t4, $t0, 0
andi	$t5, $t0, 1
# Testing zero extension
andi	$t6, $t1, 0xffff
