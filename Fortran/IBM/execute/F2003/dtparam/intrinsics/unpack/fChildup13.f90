!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*  ORIGIN                     : XLF Compiler Test,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Unpack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Uses unpack with a derived type component that extends another derived type.
!*
module m
	type base(k)
		integer, kind :: k
		integer(k) :: elem
	end type base

	type, extends(base) :: child
		integer :: element
	end type child

end module m

program a

use m
type (child(4)) :: vec(8), res(2,4), field(2,4)
logical :: mask1(8) = (/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE./)
logical :: mask2(2,4)
integer :: num
num = 1
do i=1,4
	vec(i)%element = num
	vec(i)%elem=num*2
	num = num + 1
end do
num = -1
do i = 1,4
	do j=1,2
		field(j,i)%element=num
		field(j,i)%elem=num*2
		num=num-1
	end do
end do
mask2 = reshape(mask1, (/2,4/))
res = unpack(vec,mask2,field)
!!print loop
print *, "element"
do i = 1,2
	print *, res(i,1)%element, ' ', res(i,2)%element, ' ', res(i,3)%element, ' ', res(i,4)%element
end do
print *, "elem"
do i = 1,2
	print *, res(i,1)%elem, ' ', res(i,2)%elem, ' ', res(i,3)%elem, ' ', res(i,4)%elem
end do

end program a
