.text

addiu	$t0, $zero, 0x0001
sll		$t1, $t0, 31
xori	$t2, $zero, 0
xori	$t3, $zero, 1
xori	$t4, $t0, 0
xori	$t5, $t0, 1
# Testing zero extension
xori	$t6, $t1, 0xffff
