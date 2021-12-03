.text
# Test with normal values
addi $v0, $zero, 5
addi $v1, $zero, 65
addi $t1, $zero, 0
addi $t2, $zero, 0xffff
nop
nop
nop
nop
nop
add $t0, $v0, $v1
add $t1, $v1, $v1
add $t3, $v1, $t2
add $t4, $t2, $t2
