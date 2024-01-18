!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec3_private.f
!*  PROGRAMMER                 : Gaby Baghdadi (adopted from David Nichols' 
!*                               arrsec1_private.f)
!*  DATE                       : Dec 31, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : array sections
!*  DESCRIPTION
!*
!*     Private component within derived type which dictates the displacement
!*     to array.
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
    integer, private :: a(n)
    type(ARRSC(n)) :: i(n+2)
  end type
end module

use m
integer, parameter :: NA=4, NC=6
class(A(:)), allocatable :: a1(:)
class(A(4)), allocatable :: c1(:)

allocate (A(4) :: a1(NA))
allocate (c1(NC))

a1%i(4) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NA)]
c1%i(5) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NC)]

print *, a1(:)%i(4)
print *, c1(:)%i(5)
end
