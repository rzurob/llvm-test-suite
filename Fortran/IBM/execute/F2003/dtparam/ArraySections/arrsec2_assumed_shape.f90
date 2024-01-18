!*******************************************************************************
!*  ============================================================================
!*
!*                               arrsec1_assumed_shape.f)
!*  DATE                       : Oct 13, 2008
!*  PRIMARY FUNCTIONS TESTED   : array sections
!*  DESCRIPTION
!*
!*      Test that array sections can be passed as assumed shape arrays
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module m
  type A(n)
    integer, len :: n
    real data(n)
    integer i(n)
  end type
  type B(n)
    integer, len :: n
    integer i(n)
  end type
end module

use m
integer, parameter :: NA=2,NB=4
type(A(:)), allocatable :: a1(:)
type(B(4)) :: b1(NB)

allocate (A(4) :: a1(NA))

a1%i(1) = [(i, i=1,NA)]
b1%i(2) = [(i, i=1,NB)]

call sub(a1(:)%i(1))
call sub(b1(:)%i(2))
contains
subroutine sub(i)
integer i(4:)
print *, i
end subroutine
end
