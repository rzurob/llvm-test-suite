!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Adrian Green
!*  DATE                       : July 27, 2008
!*  ORIGIN                     : XLF Compiler Test,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Pack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Uses pack with a derived type containing a pointer and
!*								 a real component.  Derived type has pointer attribute.
!*
!*
!*
!*
!*
module m1
type adrow(k)
  integer, kind :: k
  real(k),pointer :: element => null()
  real(k) :: elem
end type adrow
end module m1

program a	

use m1
type (adrow(4)), pointer :: vec(:,:) 
type (adrow(4)), pointer :: field(:)
type (adrow(4)), allocatable :: res(:)
logical :: mask1(9) = (/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .FALSE., .TRUE., .FALSE./)
logical :: mask2(3,3)
real :: num
real :: res2(3)

allocate(adrow(4) :: vec(3,3))
allocate(adrow(4) :: field(10))

num = 1.0
do i = 1,3 
	do j =1,3
		allocate(vec(j,i)%element, SOURCE = num)
		vec(j,i)%elem=num*2.0
		num = num + 1.0
	end do
end do
do i = 1,10
	allocate(field(i)%element, SOURCE = -1.0)
	field(i)%elem=-2.0
end do
mask2 = reshape(mask1, (/3, 3/))
allocate(res(ubound(pack(vec, mask2, field),1)), SOURCE = pack(vec, mask2, field))
!!print loop
!!print loop
print *, "element"
	do i=1,10
		print *, res(i)%element 
	end do
print *, "elem"
	do i =1,10
		print *, res(i)%elem
	end do
end program a
