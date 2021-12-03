.text
addiu	$t0, $zero, 1
addiu	$t1, $zero, 1

#1 << 1
sllv	$t2, $t1, $t0
addiu	$t1, $t1, 1

#1 << 2
sllv	$t3, $t1, $t0
addiu	$t1, $t1, 1

#1 << 3
sllv	$t4, $t1, $t0
addiu	$t1, $t1, 1

#1 << 4
sllv	$t5, $t1, $t0
addiu	$t1, $t1, 1

#1 << 5
sllv	$t6, $t1, $t0
addiu	$t1, $t1, 1

#1 << 6
sllv	$t7, $t1, $t0
addiu	$t1, $t1, 1

#1 << 7
sllv	$t8, $t1, $t0
addiu	$t1, $t1, 1

#1 << 8
sllv	$v0, $t1, $t0
addiu	$t1, $t1, 1

#1 << 9
sllv	$v1, $t1, $t0
addiu	$t1, $t1, 1

#1 << 10
sllv	$a0, $t1, $t0
addiu	$t1, $t1, 1

#1 << 11
sllv	$a1, $t1, $t0
addiu	$t1, $t1, 1

#1 << 12
sllv	$a2, $t1, $t0
addiu	$t1, $t1, 1

#1 << 13
sllv	$a3, $t1, $t0
