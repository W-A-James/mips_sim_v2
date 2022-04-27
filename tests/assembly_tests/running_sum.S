.text
.global __start
nop
nop

__start:
  addiu $t0, $0, 1000
  addu  $a0, $0, $0

sum_loop:
  addu  $a0, $a0, $t0
  sub   $t0, $t0, 1
  bgt   $t0, $0, sum_loop

done:
  .word 0xDEADBEEF

.data
.word 0
