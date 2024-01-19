!*******************************************************************************
!*  ============================================================================
!*
!*                               arrsec1_assumed_size.f)
!*  DATE                       : Dec 31, 2008
!*  PRIMARY FUNCTIONS TESTED   : array sections
!*  DESCRIPTION
!*
!*      Test that assumed size arrays and sections of assumed size arrays
!*      function correctly when the actual arguments are array sections.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module m
  type ARRSC(n)
    integer, len :: n
    integer i
    real a(n)
  end type
  type A(n)
    integer, len :: n
    real data(n)
    type(ARRSC(n)) :: i(n)
  end type

  contains

    subroutine sub(i)
    type(ARRSC(4)) i(*)
    print *, i(2:4)
    end subroutine

end module

use m
integer, parameter :: NA=4, NB=6
real r
type(A(:)), allocatable :: a1(:)
type(A(4)) :: b1(NB)

allocate (A(4) :: a1(NA))

a1%i(1) = [(ARRSC(4)(i,[(r,r=1,4)]),i=1,NA)]
b1%i(2) = [(ARRSC(4)(i,[(r,r=1,4)]),i=1,NB)]

call sub(a1(:)%i(1))
call sub(b1(:)%i(2))

call sub(a1%i(1))
call sub(b1%i(2))
end
