!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec2_assumed_size.f
!*                               arrsec1_assumed_size.f)
!*  DATE                       : Oct 13, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : array sections
!*  DESCRIPTION
!*
!*      Test that assumed size arrays and sections of assumed size arrays
!*      function correctly when the actual arguments are array sections.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module m
  type A(n)
    integer, len :: n
    real data(n)
    integer i(n)
  end type
end module

use m
integer, parameter :: NA=4, NB=6
type(A(:)), allocatable :: a1(:)
type(A(4)) :: b1(NB)

allocate (A(4) :: a1(NA))

a1%i(1) = [(i, i=1,NA)]
b1%i(2) = [(i, i=1,NB)]

call sub(a1(:)%i(1))
call sub(b1(:)%i(2))

call sub(a1%i(1))
call sub(b1%i(2))
end

subroutine sub(i)
integer i(*)
print *, i(2:4)
end subroutine
