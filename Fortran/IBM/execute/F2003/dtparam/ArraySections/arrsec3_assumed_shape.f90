!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec3_assumed_shape.f 
!*  PROGRAMMER                 : Gaby Baghdadi (adopted from David Nichols' 
!*                               arrsec1_assumed_shape.f)
!*  DATE                       : Dec 31, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : array sections
!*  DESCRIPTION
!*
!*      Test that array sections can be passed as assumed shape arrays
!*      
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
integer, parameter :: NA=2,NB=4
real r
type(A(:)), allocatable :: a1(:)
type(B(4)) :: b1(NB)

allocate (A(4) :: a1(NA))

a1%i(1) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NA)]
b1%i(2) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NB)]

call sub(a1(:)%i(1))
call sub(b1(:)%i(2))
contains
subroutine sub(i)
type(ARRSC(4)) i(4:)
print *, i
end subroutine
end
