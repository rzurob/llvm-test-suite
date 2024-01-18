!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec2_explicit.f 
!*  PROGRAMMER                 : Gaby Baghdadi (adopted from David Nichols' 
!*                               arrsec1_explicit_shape.f)
!*  DATE                       : Oct 13, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : Array sections
!*  DESCRIPTION
!*
!*      Test array sections of explicit shaped arrays within derived types
!*      Shape is known at compile time.
!*      
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
integer, parameter :: NA=5, NB=6
type(A(4)) :: a1(NA)
type(B(4)) :: b1(NB)

a1%i(1) = [(i, i=1,NA)]
b1%i(2) = [(i, i=1,NB)]

print *, a1(:)%i(1)
print *, b1(:)%i(2)
end
