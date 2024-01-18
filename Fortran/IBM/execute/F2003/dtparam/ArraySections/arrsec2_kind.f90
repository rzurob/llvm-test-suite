!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec2_kind.f
!*  PROGRAMMER                 : Gaby Baghdadi (adopted from David Nichols' 
!*                               arrsec1_kind.f)
!*  DATE                       : Oct 13, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : array section
!*  DESCRIPTION
!*
!*      Array sections of derived types with kind type parameter
!*      
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module m
  type A(n)
    integer, kind :: n
    integer a(n)
    integer i(n)
  end type
  type B(n)
    integer, kind :: n
    integer i(n)
  end type
end module

use m

integer, parameter :: NA=4, NB=5
type(A(4)), allocatable :: a1(:)
type(B(4)), allocatable :: b1(:)
allocate (a1(NA))
allocate (b1(NB))

a1%i(1) = [(i, i=1,NA)]
b1%i(2) = [(i, i=1,NB)]

print *, a1(:)%i(1)
print *, b1(:)%i(2)
end
