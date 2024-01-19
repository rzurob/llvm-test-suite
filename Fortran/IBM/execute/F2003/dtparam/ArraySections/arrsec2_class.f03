!*******************************************************************************
!*  ============================================================================
!*
!*                               arrsec1_class.f)
!*  DATE                       : Oct 13, 2008
!*  PRIMARY FUNCTIONS TESTED   : Array sections
!*  REQUIRED COMPILER OPTIONS  :
!*  DESCRIPTION
!*
!*      Array sections contained within polymorphic types
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module m
  type A(n)
    integer, len :: n
    real data(n)
    integer i(n,4)
  end type
  type B(n)
    integer, len :: n
    integer i(n,4)
  end type
end module

use m
integer, parameter :: NA=4,NB=5,NC=6,ND=7
class(A(:)), allocatable :: a1(:,:)
class(B(:)), allocatable :: b1(:,:)
class(A(4)), allocatable :: c1(:,:)
class(B(4)), allocatable :: d1(:,:)

allocate (A(4) :: a1(NA,NA))
allocate (B(4) :: b1(NB,NB))
allocate (c1(NC,NC))
allocate (d1(ND,ND))

a1(:,:)%i(1,1) = 9
a1(:,NA)%i(1,1) = [(i, i=1,NA)]

b1(:,:)%i(2,2) = 8
b1(:,NB)%i(2,2) = [(i, i=1,NB)]

c1(:,:)%i(3,3) = 7
c1(:,NC)%i(3,3) = [(i, i=1,NC)]

d1(:,:)%i(4,4) = 6
d1(:,ND)%i(4,4) = [(i, i=1,ND)]

a1(1,:)%i(1,1) = [(i, i=1,NA)]
b1(1,:)%i(2,2) = [(i, i=1,NB)]
c1(1,:)%i(3,3) = [(i, i=1,NC)]
d1(1,:)%i(4,4) = [(i, i=1,ND)]

print *, a1(:,:)%i(1,1)
print *, b1(:,:)%i(2,2)
print *, c1(:,:)%i(3,3)
print *, d1(:,:)%i(4,4)
end
