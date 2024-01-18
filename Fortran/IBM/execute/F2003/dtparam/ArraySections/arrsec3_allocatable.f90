!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec3_allocatable.f
!*                               arrsec1_allocatable.f)
!*  DATE                       : Dec 31, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : Array Sections
!*  DESCRIPTION
!*
!*      Deferred length parameters
!*      Deferred size array components
!*      with varying displacements into a type
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
    type(ARRSC(n)) :: i(n)
  end type
  type B(n)
    integer, len :: n
    integer a(n)
    type(ARRSC(n)) :: i(n)
  end type
  type C(n)
    integer, len :: n
    integer a(n)
    integer b(n)
    type(ARRSC(n)) :: i(n)
  end type
end module

use m
integer, parameter :: NA=1, NB=2, NC=4
real r
type(A(:)), allocatable :: a1(:)
type(B(:)), allocatable :: b1(:)
type(C(4)), allocatable :: c1(:)

allocate (A(4) :: a1(NA))
allocate (B(4) :: b1(NB))
allocate (C(4) :: c1(NC))

a1(:)%i(1) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NA)]
b1(2:)%i(1) = [(ARRSC(4)([(r,r=1,4)],i),i=2,NB)]
c1(:NC-1)%i(1) = [(ARRSC(4)([(r,r=1,4)],i),i=1,NC-1)]

print *, a1(:)%i(1)
print *, b1(2:)%i(1)
print *, c1(:NC-1)%i(1)
end
