!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2006-11-02
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : statement function invocations as ac_values
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : statement function
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Use a statement function identical to the constructor for a derived type in
!*  AC's, verifying the values.
!*  We print and assign the values, to test two different types of use.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt43mod

  implicit none

  type derived
     integer :: val = 4, v2 = 6
  end type derived

end module acetdt43mod


program acetdt43

  use acetdt43mod
  implicit none

  type (derived) :: derived, darr(1)

  integer(4)     :: i

  ! The statement function:
  derived(i) = d2(-i,-6) ! can't refer directly to structure constructor, due to name conflict

  print *, [derived(3)], [derived:: derived(3)], [derived:: (derived(3), i=1,1)]

  darr  = [derived:: derived(3)]

  if (darr(1) % val /= -3 .or. darr(1) % v2 /= -6) stop 2

  darr  = [derived:: (derived(3), i=1,1)]

  if (darr(1) % val /= -3 .or. darr(1) % v2 /= -6) stop 3

  darr  = [derived:: (derived(i), i=1,1)]

  if (darr(1) % val /= -1 .or. darr(1) % v2 /= -6) stop 4

contains

  type (derived) function d2(i,j)
    integer :: i, j
    d2 % val = i
    d2 % v2  = j
  end function d2

end program acetdt43
