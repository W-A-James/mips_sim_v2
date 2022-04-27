.text

addi	  $t3, $zero, 9	  # n
addi    $t4, $zero, 1     # constant 1
addi	  $t0, $zero, 1	  # Current result
move    $t1, $zero        # Previous result
move    $t2, $zero        # temp

# check if n == 0 or 1
nop
beq		  $t3, $zero, case_n_0
nop

case_n_1:
    nop
	  bne		$t3, $t4, fib_loop      # if n != 1: goto fib_loop
	  nop
    beq     $zero, $zero, done      # goto done
    nop

case_n_0:
    move    $t0, $zero              # current = 0
    nop
    beq     $zero, $zero, done      # goto done
    nop

fib_loop:
    nop 
    beq     $t3, $zero, done
    nop
    addi    $t3, $t3, -1            # n = n - 1
    move    $t2, $t0                # tmp = current
    add     $t0, $t0, $t1           # current = current + prev
    move    $t1, $t2                # prev = tmp
    nop
    beq     $zero, $zero, fib_loop  #  goto fib_loop
    nop

done:
	nop
.word 0XDEADBEEF 
