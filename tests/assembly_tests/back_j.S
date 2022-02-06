.text

li	$t0, 100
li	$t1, 100
nop
j	skip
nop

done:
add $t0, $t1, $t0
.word 0xdeadbeef

skip:
add $t1, $t1, $t1
nop
j done
nop
