!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Pack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Uses unpack with a derived type containing a pointer and
!*                              a real component with a 3x3 array.
!*

module m1
type adrow(k)
  integer, kind :: k
  real(k), pointer :: element => null()
  real(k) :: elem
end type adrow
end module m1

program a

use m1
type (adrow(4)) :: vec(6)
type (adrow(4)) :: field1(9)
type (adrow(4)) :: res(3,3), field2(3,3)
logical :: mask1(9) = (/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .FALSE., .TRUE., .FALSE./)
logical :: mask2(3,3)
real :: num
real :: res2(3)
num = 1.0
do i = 1,6
	allocate(vec(i)%element, SOURCE = num)
	vec(i)%elem=num*2.0
	num = num + 1.0
end do
do j = 1,9
	allocate (field1(j)%element, SOURCE = -1.0)
	field1(j)%elem = -5.0
end do
mask2 = reshape(mask1, (/3, 3/))
field2 = reshape(field1, (/3, 3/))
res = unpack(vec, mask2, field2)
!!print loop
do k =1,3
	print *, res(k,1)%element, '  ', res(k,2)%element, '  ', res(k,3)%element
end do
print *, ' '
do k =1,3
	print *, res(k,1)%elem, '  ', res(k,2)%elem, '  ', res(k,3)%elem
end do
end program a