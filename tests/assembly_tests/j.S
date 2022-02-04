.text

li	$t0, 100
li	$t1, 100
nop
j	skip
nop
add $t0, $t1, $t0

skip:
add $t1, $t1, $t1
nop
.word 0xdeadbeef
