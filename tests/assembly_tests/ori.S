.text

addiu   $t0, $zero, 1
lui		$t1, 0x8000
ori		$t2, $zero, 0
ori		$t3, $zero, 1
ori		$t4, $t0, 0
ori		$t5, $t0, 1
#Testing zero extension
ori		$t6, $t1, 0xffff
