!*******************************************************************************
!*  ============================================================================
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

module acetdt62mod

  implicit none
  type dt
     real :: val
  end type dt

end module acetdt62mod


program acetdt62

  use acetdt62mod
  implicit none
  type(dt), pointer :: a(:), b(:), c(:)
  type(dt), allocatable, target :: base(:)
  integer :: i

  base = [dt:: (dt(real(i/10.0)), i=11,27)] ! 1.1, 1.2, 1.3, ..., 2.7 - 17 items

  print *, base

  a     => base(5:)       ! Lower bound of a is 1
  b(5:) => base(5:)       ! Lower bound of b is 5
  print *, [dt:: base(5:9)]
  print *, [dt:: b(5:9)]
  print *, [dt:: a(1:5)]
  a = [dt:: b]
  print *, base
  a = [dt:: b(ubound(b,1):lbound(b,1):-1)]
  print *, base
  print *, [dt:: (a(i),b(i), i=5,12)]

  b ([integer:: (int(10*b(i)%val+0.1)-10, i=5,10)]) = a ([integer:: (int(10*a(i)%val+0.1)-14, i=6,1,-1)])
  print *, base

end program acetdt62
