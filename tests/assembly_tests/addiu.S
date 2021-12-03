.text
# Test with normal values
addi $v0, $zero, 5
addi $v1, $zero, 5
# We do this since we don't have the hazard detection enabled
nop
nop
nop
nop
nop
nop

# Test overflow
#                   -1
addiu $v1, $v1, 0xffff
