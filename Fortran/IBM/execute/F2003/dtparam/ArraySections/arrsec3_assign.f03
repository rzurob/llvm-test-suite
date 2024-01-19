!*******************************************************************************
!*  ============================================================================
!*
!*                               arrsec1_assign.f)
!*  DATE                       : Dec 31, 2008
!*  PRIMARY FUNCTIONS TESTED   : Array section assignment
!*  DESCRIPTION
!*
!*      Test array sections generate the proper store operations
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
    integer a(n)
    type(ARRSC(n)) :: i(2*n)
  end type
end module

use m
integer, parameter :: NA=2, NC=3
real r
type(A(:)), allocatable :: a1(:)
type(A(4)), allocatable :: c1(:)

allocate (A(4) :: a1(NA))
allocate (c1(NC))

a1(:)%i(1) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NA)]
c1(:)%i(2) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NC)]

print *, a1(:)%i(1)
print *, c1(:)%i(2)
end
