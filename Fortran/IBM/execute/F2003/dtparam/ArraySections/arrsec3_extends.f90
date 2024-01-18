!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : arrsec3_extends.f
!*  PROGRAMMER                 : Gaby Baghdadi (adopted from David Nichols' 
!*                               arrsec1_extends.f)
!*  DATE                       : Dec 31, 2008
!*  ORIGIN                     : XLF Development
!*  PRIMARY FUNCTIONS TESTED   : Array sections
!*  DESCRIPTION
!*
!*      Test array sections from extended derived types.
!*      
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module m
  type ARRSC(n)
    integer, len :: n
    integer a(n)
    integer i
  end type
  type Z(n)
    integer, len :: n
    integer a(n)
  end type
  type X(n)
    integer, len :: n
    type(ARRSC(n)) :: i(n)
  end type

  type, extends(Z) :: A(m)
    integer, len :: m
    type(ARRSC(m)) :: i(m)
  end type
  type, extends(X) :: B(m)
    integer, len :: m
  end type
end module

use m
integer, parameter :: NA=7, NB=6, NC=5, ND=4
integer j
class(A(:,:)), allocatable :: a1(:)
class(B(:,:)), allocatable :: b1(:)
class(A(4,4)), allocatable :: c1(:)
class(B(4,4)), allocatable :: d1(:)

allocate (A(4,4) :: a1(NA))
allocate (B(4,4) :: b1(NB))
allocate (c1(NC))
allocate (d1(ND))

a1%i(1) = [(ARRSC(4)([(j,j=1,4)],i),i=1,NA)]
b1%i(2) = [(ARRSC(4)([(j,j=1,4)],i),i=1,NB)]
c1%i(3) = [(ARRSC(4)([(j,j=1,4)],i),i=1,NC)]
d1%i(4) = [(ARRSC(4)([(j,j=1,4)],i),i=1,ND)]

print *, a1%i(1)
print *, b1%i(2)
print *, c1%i(3)
print *, d1%i(4)
end
