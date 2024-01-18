!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec3_pointer.f
!*                               arrsec1_pointer.f)
!*  DATE                       : Dec 31, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : array sections
!*  DESCRIPTION
!*
!*      Deferred length type parameters with pointers to derived types
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
integer, parameter :: NA=4, NB=6
real r
type(A(4)), target :: a1t(NA)
type(B(4)), target :: b1t(NB)

type(A(:)), pointer :: a1p(:)
type(B(:)), pointer :: b1p(:)

a1p => a1t
b1p => b1t

a1p%i(1) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NA)]
b1p%i(2) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NB)]

print *, a1p(:)%i(1)
print *, b1p(:)%i(2)
end
