!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec3_lenlen.f
!*                               arrsec1_lenlen.f)
!*  DATE                       : Dec 31, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   :
!*  REQUIRED COMPILER OPTIONS  :
!*  DESCRIPTION
!*
!*      Deferred length type parameters in use with multi dimensional arrays
!*
!* =============================================================================
!2325678901232567890123256789012325678901232567890123256789012325678901232567890

module m

  type ARRSC(n)
    integer, len :: n
    integer a(n)
    integer i
  end type

  type A(n)
    integer, len :: n
    real b(n)
    type(ARRSC(n)) :: i(n,n,n)
  end type

  type B(n)
    integer, len :: n
    real a(n)
    type(ARRSC(n)) :: i(n,n)
  end type

  type C(n)
    integer, len :: n
    type(ARRSC(n)) :: i(n,n)
  end type

end module

use m
integer, parameter :: NA=4, NB=5, NC=6
integer j
type(A(:)), allocatable :: a1(:,:,:)
type(B(:)), allocatable :: b1(:,:)
type(C(:)), allocatable :: c1(:,:)
type(A(4)), allocatable :: a2(:,:,:)
type(B(4)), allocatable :: b2(:,:)
type(C(4)), allocatable :: c2(:,:)

allocate (A(4) :: a1(NA,2,2))
allocate (B(4) :: b1(NB,2))
allocate (C(4) :: c1(NC,2))
allocate (a2(NA,2,2))
allocate (b2(NB,2))
allocate (c2(NC,2))

a1(:,:,:)%i(1,1,1) = reshape([(ARRSC(4)([(j,j=1,4)],9),i=1,NA*2*2)],[NA,2,2])
a1(:,2,2)%i(1,1,1) = [(ARRSC(4)([(j,j=1,4)],i),i=1,NA)]

b1(:,:)%i(2,2)    = reshape([(ARRSC(4)([(j,j=1,4)],8),i=1,NB*2)],[NB,2])
b1(:,2)%i(2,2)    = [(ARRSC(4)([(j,j=1,4)],i),i=1,NB)]

c1(:,:)%i(3,3)    = reshape([(ARRSC(4)([(j,j=1,4)],7),i=1,NC*2)],[NC,2])
c1(:,2)%i(3,3)    = [(ARRSC(4)([(j,j=1,4)],i),i=1,NC)]

a2(:,:,:)%i(1,1,1)= reshape([(ARRSC(4)([(j,j=1,4)],6),i=1,NA*2*2)],[NA,2,2])
a2(:,2,2)%i(1,1,1) = [(ARRSC(4)([(j,j=1,4)],i),i=1,NA)]

b2(:,:)%i(2,2)     = reshape([(ARRSC(4)([(j,j=1,4)],5),i=1,NB*2)],[NB,2])
b2(:,2)%i(2,2)     = [(ARRSC(4)([(j,j=1,4)],i),i=1,NB)]

c2(:,:)%i(3,3)     = reshape([(ARRSC(4)([(j,j=1,4)],4),i=1,NC*2)],[NC,2])
c2(:,2)%i(3,3)     = [(ARRSC(4)([(j,j=1,4)],i),i=1,NC)]

a1(1,1,:)%i(1,1,1) = [(ARRSC(4)([(j,j=1,4)],i),i=1,2)]
b1(1,:)%i(2,2)     = [(ARRSC(4)([(j,j=1,4)],i),i=1,2)]
c1(1,:)%i(3,3)     = [(ARRSC(4)([(j,j=1,4)],i),i=1,2)]
a2(1,1,:)%i(1,1,1) = [(ARRSC(4)([(j,j=1,4)],i),i=1,2)]
b2(1,:)%i(2,2)     = [(ARRSC(4)([(j,j=1,4)],i),i=1,2)]
c2(1,:)%i(3,3)     = [(ARRSC(4)([(j,j=1,4)],i),i=1,2)]

print *, a1(:,:,:)%i(1,1,1)
print *, b1(:,:)%i(2,2)
print *, c1(:,:)%i(3,3)
print *, a2(:,:,:)%i(1,1,1)
print *, b2(:,:)%i(2,2)
print *, c2(:,:)%i(3,3)
end
