.text

addi $t0, $zero, 0xFFFF
addi $t1, $zero, 0x6000
addi $t2, $zero, 0x2000
addi $t3, $zero, 0x080F
addi $t4, $zero, 0x0000
nop
clz  $t0, $t0 # should be 0
clz  $t1, $t1 # should be 17
clz  $t2, $t2 # should be 18
clz  $t3, $t3 # should be 20
clz  $t4, $t4 # should be 32
