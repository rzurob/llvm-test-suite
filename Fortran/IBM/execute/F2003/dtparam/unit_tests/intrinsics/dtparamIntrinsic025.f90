!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTIONS               : Testing MERGE intrinsic function
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none

type base1(k, m)
   integer,  kind :: k
   integer, len  :: m
   integer(kind=4), allocatable :: int(:,:)
   real(kind=k) :: r(m)
end type

type base2(k, m)
   integer,  kind :: k
   integer, len  :: m
   integer(kind=4), allocatable :: int(:,:)
   real(kind=k-4) :: r(m)
end type

logical, parameter :: T=.true., F=.false.
logical, parameter :: mask(8)=reshape((/F,T,F,T,F,T,F,T/), (/8/))
integer i, j, k(3,6)
type(base1(4, :)), pointer :: b11
type(base1(4, 8)), target ::  b12
type(base2(8, :)), allocatable :: b21
type(base2(8, 8)), target ::  b22

k=reshape((/((i+j,i = 1, 3),j = 1, 6 )/),(/3,6/))
allocate (b12%int(3,6), source = k)
b12%r = (/((sqrt(real(i))*sqrt(real(i))), i = 1, 8)/)
b22%r = (/((real(i)*real(i)), i = 1, 8)/)
b11=>b12
allocate(b22%int(6,3),source = reshape(k, (/6,3/)))
print *, matmul(b22%int, b12%int)
print *, merge(b22%r, b12%r, mask)
end
