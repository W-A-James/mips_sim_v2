.text

lui	 $t0, 0x8000
addi $t1, $zero, 4
srav $t0, $t0, $t1
srav $t0, $t0, $t1
srav $t0, $t0, $t1 # result in $t0 should be 0xfff8_0000
