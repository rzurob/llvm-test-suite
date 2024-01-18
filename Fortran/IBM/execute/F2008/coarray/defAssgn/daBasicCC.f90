!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : daBasicCC
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-12-20
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 CAF coarray defined assignment
!*  SECONDARY FUNCTIONS TESTED : simple test of same type but mismatched rank, dummy args are coarrays
!*
!*  DESCRIPTION
!*
!*  Verify that defined assignment on simple integer coarrays with mismatched
!*  rank is done correctly.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod

  interface assignment ( = )
     module procedure intAssign
  end interface

contains

  subroutine intAssign(x, y)
    integer, intent(out) :: x[*]
    integer, intent(in) :: y(:)[*]
    print *, "x=", x, "becomes sum(", y, ")"
    x = sum(y)
  end subroutine intAssign

end module mod

program daBasicCC

  use :: mod
  integer, save :: c1[*], c2(3)[*]
  integer :: v1, v2(3), i

  c2 = [(this_image()+i, i=-1,1)]  ! matched rank - no defined assignment
  c1 = c2                          ! defined assignment expected
  print *, c1, c2

  v2 = [(this_image()+i, i=-1,1)]  ! repeat with non-coarray variable
  v1 = sum(v2)                     ! no defined assignment, but this is the expected value
  print *, v1, v2

  if (v1 /= c1 ) error stop 2
  if (any(v2 /= c2)) error stop 3

end program daBasicCC
