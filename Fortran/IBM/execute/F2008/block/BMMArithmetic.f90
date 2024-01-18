!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-12-14
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : mixed-mode arithmetic
!*
!*  DESCRIPTION
!*
!*  Try using arithmetic on variables defined one way in the main program and
!*  redefined in the block, verifying that the correct operations are used
!*  (I<op>I, R<op>I, R<op>R, I<op>R, for <op> in +, -, *, /, <, >).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BMMArithmetic

  implicit none
  integer(4) :: v1, v2
  real(4) :: v3, v4

  v1 = 20
  v2 = 3
  v3 = 1.1
  v4 = 9.9

  print *, v1, v2, v3, v4
  print *, v1 + v2, v1 - v2, v1 * v2, v1 / v2 ! I::I
  print *, v3 + v2, v3 - v2, v3 * v2, v3 / v2 ! R::I
  print *, v3 + v4, v3 - v4, v3 * v4, v3 / v4 ! R::R
  print *, v1 + v4, v1 - v4, v1 * v4, v1 / v4 ! I::R

  ! Now swap real and integer:
  block
    integer(4) :: v3, v4
    real(4) :: v1, v2
    v1 = 9.9
    v2 = 6.6
    v3 = 22
    v4 = 7
    print *, v1, v2, v3, v4
    print *, v1 + v2, v1 - v2, v1 * v2, v1 / v2 ! R::R
    print *, v3 + v2, v3 - v2, v3 * v2, v3 / v2 ! I::R
    print *, v3 + v4, v3 - v4, v3 * v4, v3 / v4 ! I::I
    print *, v1 + v4, v1 - v4, v1 * v4, v1 / v4 ! R::I
  end block

  ! Now use some host-associated and some local:
  block
    integer(4) :: v3
    real(4) :: v1
    v1 = 9.9
    v3 = 22
    print *, v1, v2, v3, v4
    print *, v1 + v2, v1 - v2, v1 * v2, v1 / v2 ! R::I
    print *, v3 + v2, v3 - v2, v3 * v2, v3 / v2 ! I::I
    print *, v3 + v4, v3 - v4, v3 * v4, v3 / v4 ! I::R
    print *, v1 + v4, v1 - v4, v1 * v4, v1 / v4 ! R::R
  end block

  print *, v1, v2, v3, v4

end program BMMArithmetic
