!*******************************************************************************
!*  ============================================================================
!*
!*                               arrsec1_explicit_shape.f)
!*  DATE                       : Dec 31, 2008
!*  PRIMARY FUNCTIONS TESTED   : Array sections
!*  DESCRIPTION
!*
!*      Test array sections of explicit shaped arrays within derived types
!*      Shape is known at compile time.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module m
  type ARRSC(n)
    integer, len :: n
    real a(n)
    integer i
  end type
  type A(n)
    integer, len :: n
    real data(n)
    type(ARRSC(n)) :: i(n)
  end type
  type B(n)
    integer, len :: n
    type(ARRSC(n)) :: i(n)
  end type
end module

use m
integer, parameter :: NA=5, NB=6
real r
type(A(4)) :: a1(NA)
type(B(4)) :: b1(NB)

a1%i(1) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NA)]
b1%i(2) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NB)]

print *, a1(:)%i(1)
print *, b1(:)%i(2)
end
