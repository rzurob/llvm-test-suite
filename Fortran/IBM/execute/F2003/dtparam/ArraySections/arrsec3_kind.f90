!*******************************************************************************
!*  ============================================================================
!*
!*                               arrsec1_kind.f)
!*  DATE                       : Dec 31, 2008
!*  PRIMARY FUNCTIONS TESTED   : array section
!*  DESCRIPTION
!*
!*      Array sections of derived types with kind type parameter
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module m
  type ARRSC(n)
    integer, kind :: n
    real(n) a(n)
    integer(n) i
  end type
  type A(n)
    integer, kind :: n
    integer(n) :: a(n)
    type(ARRSC(n)) :: i(n)
  end type
  type B(n)
    integer, kind :: n
    type(ARRSC(n)) :: i(n)
  end type
end module

use m

integer, parameter :: NA=4, NB=5
real r
type(A(8)), allocatable :: a1(:)
type(B(4)), allocatable :: b1(:)
allocate (a1(NA))
allocate (b1(NB))

a1%i(1) = [(ARRSC(8)([(r,r=1,8)],i),i=1,NA)]
b1%i(2) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NB)]

print *, a1(:)%i(1)
print *, b1(:)%i(2)
end
