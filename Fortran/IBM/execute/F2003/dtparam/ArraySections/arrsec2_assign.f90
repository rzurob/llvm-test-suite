!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec2_assign.f
!*                               arrsec1_assign.f)
!*  DATE                       : Oct 13, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : Array section assignment
!*  DESCRIPTION
!*
!*      Test array sections generate the proper store operations
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module m
  type A(n)
    integer, len :: n
    integer a(n)
    integer i(2*n)
  end type
end module

use m
integer, parameter :: NA=2, NC=3
type(A(:)), allocatable :: a1(:)
type(A(4)), allocatable :: c1(:)

allocate (A(4) :: a1(NA))
allocate (c1(NC))

a1(:)%i(1) = [(i, i=1,NA)]
c1(:)%i(2) = [(i, i=1,NC)]

print *, a1(:)%i(1)
print *, c1(:)%i(2)
end
