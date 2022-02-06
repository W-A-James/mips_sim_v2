.text
li      $t0, 10
addiu   $t1, $0, 20
jal     add
li      $t2, 30
nop
nop
nop

add:
add     $t0, $t0, $t0
.word   0xdeadbeef
