!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec3_class.f
!*                               arrsec1_class.f)
!*  DATE                       : Dec 31, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : Array sections
!*  REQUIRED COMPILER OPTIONS  :
!*  DESCRIPTION
!*
!*      Array sections contained within polymorphic types
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module m
  type ARRSC(n)
    integer, len :: n
    integer a(n)
    integer i
  end type
  type A(n)
    integer, len :: n
    real data(n)
    type(ARRSC(n)) :: i(n,4)
  end type
  type B(n)
    integer, len :: n
    type(ARRSC(n)) :: i(n,4)
  end type
end module

use m
integer, parameter :: NA=4,NB=5,NC=6,ND=7
integer j
class(A(:)), allocatable :: a1(:,:)
class(B(:)), allocatable :: b1(:,:)
class(A(4)), allocatable :: c1(:,:)
class(B(4)), allocatable :: d1(:,:)

allocate (A(4) :: a1(NA,NA))
allocate (B(4) :: b1(NB,NB))
allocate (c1(NC,NC))
allocate (d1(ND,ND))

a1(:,:)%i(1,1) = reshape([(ARRSC(4)([(j,j=1,4)],9),i=1,NA*NA)],[NA,NA])
a1(:,NA)%i(1,1) = [(ARRSC(4)([(j,j=1,4)],i),i=1,NA)]

b1(:,:)%i(2,2) = reshape([(ARRSC(4)([(j,j=1,4)],8),i=1,NB*NB)],[NB,NB])
b1(:,NB)%i(2,2) = [(ARRSC(4)([(j,j=1,4)],i),i=1,NB)]

c1(:,:)%i(3,3) = reshape([(ARRSC(4)([(j,j=1,4)],7),i=1,NC*NC)],[NC,NC])
c1(:,NC)%i(3,3) = [(ARRSC(4)([(j,j=1,4)],i),i=1,NC)]

d1(:,:)%i(4,4) = reshape([(ARRSC(4)([(j,j=1,4)],6),i=1,ND*ND)],[ND,ND])
d1(:,ND)%i(4,4) = [(ARRSC(4)([(j,j=1,4)],i),i=1,ND)]

a1(1,:)%i(1,1) = [(ARRSC(4)([(j,j=1,4)],i),i=1,NA)]
b1(1,:)%i(2,2) = [(ARRSC(4)([(j,j=1,4)],i),i=1,NB)]
c1(1,:)%i(3,3) = [(ARRSC(4)([(j,j=1,4)],i),i=1,NC)]
d1(1,:)%i(4,4) = [(ARRSC(4)([(j,j=1,4)],i),i=1,ND)]

print *, a1(:,:)%i(1,1),'\n'
print *, b1(:,:)%i(2,2),'\n'
print *, c1(:,:)%i(3,3),'\n'
print *, d1(:,:)%i(4,4),'\n'
end
