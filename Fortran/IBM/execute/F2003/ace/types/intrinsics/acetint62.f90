!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint62
!*
!*  DATE                       : 2006-11-26
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : pointer with specified lower bound in AC
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Include references to pointers with specified lower bound on both LHS and
!*  RHS of assignment, within AC's.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint62

  implicit none
  real,pointer :: a(:),b(:),c(:)
  real,allocatable,target :: base(:)
  integer :: i

  base = [real:: (real(i/10.0), i=11,27)] ! 1.1, 1.2, 1.3, ..., 2.7 - 17 items

  print *, base

  a     => base(5:)       ! Lower bound of a is 1
  b(5:) => base(5:)       ! Lower bound of b is 5
  print *, [real:: base(5:9)]
  print *, [real:: b(5:9)]
  print *, [real:: a(1:5)]
  a = [real:: b]
  print *, base
  a = [real:: b(ubound(b,1):lbound(b,1):-1)]
  print *, base
  print *, [real:: (a(i),b(i), i=5,12)]

  b ([integer:: (int(10*b(i)+0.1)-10, i=5,10)]) = a ([integer:: (int(10*a(i)+0.1)-14, i=6,1,-1)])
  print *, base

end program acetint62
