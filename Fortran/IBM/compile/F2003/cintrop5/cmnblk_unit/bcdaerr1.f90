! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : June. 20, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common block members
!*                              : should not be appeared as members
!*                              : of equivalence statements, or
!*                              : pointers, or none-interoperable
!*                              : variables, or zero-sized arrays.
!234567890123456789012345678901234567890123456789012345678901234567890
program bcdaerr1
implicit none
integer, pointer :: x
character*20 y
real z1, z2(2,2), z3(1,0)
equivalence(z1, z2(1,2))
common /blk/ x, y, z1, z3
bind(c) /blk/
end program
