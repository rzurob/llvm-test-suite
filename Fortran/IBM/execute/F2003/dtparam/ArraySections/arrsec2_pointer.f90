!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec2_pointer.f
!*                               arrsec1_pointer.f)
!*  DATE                       : Oct 13, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : array sections
!*  DESCRIPTION
!*
!*      Deferred length type parameters with pointers to derived types
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
integer, parameter :: NA=4, NB=6
type(A(4)), target :: a1t(NA)
type(B(4)), target :: b1t(NB)

type(A(:)), pointer :: a1p(:)
type(B(:)), pointer :: b1p(:)

a1p => a1t
b1p => b1t

a1p%i(1) = [(i, i=1,NA)]
b1p%i(2) = [(i, i=1,NB)]

print *, a1p(:)%i(1)
print *, b1p(:)%i(2)
end
