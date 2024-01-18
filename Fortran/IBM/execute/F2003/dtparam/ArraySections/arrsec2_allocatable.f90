!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec2_allocatable.f
!*                               arrsec1_allocatable.f)
!*  DATE                       : Oct 13, 2008
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
  type A(n)
    integer, len :: n
    integer i(n)
  end type
  type B(n)
    integer, len :: n
    integer a(n)
    integer i(n)
  end type
  type C(n)
    integer, len :: n
    integer a(n)
    integer b(n)
    integer i(n)
  end type
end module

use m
integer, parameter :: NA=1, NB=2, NC=4
type(A(:)), allocatable :: a1(:)
type(B(:)), allocatable :: b1(:)
type(C(:)), allocatable :: c1(:)

allocate (A(4) :: a1(NA))
allocate (B(4) :: b1(NB))
allocate (C(4) :: c1(NC))

a1(:)%i(1) = [(i, i=1,NA)]
b1(2:)%i(1) = [(i, i=2,NB)]
c1(:NC-1)%i(1) = [(i, i=1,NC-1)]

print *, a1(:)%i(1)
print *, b1(2:)%i(1)
print *, c1(:NC-1)%i(1)
end
