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
!*  DESCRIPTION                : Uses pack with a derived type component that extends another derived type. 
!*								
!*
!*
!*
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
type (child(4)) :: vec(2,4), res(8), field(8) 
logical :: mask1(8) = (/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE./)
logical :: mask2(2,4)
integer :: num
num = 1
do k = 1,2
	do j = 1,4
		vec(k,j)%element = num
		vec(k,j)%elem = num*2
		num = num + 1
	end do
end do
num = -1
do i = 1,8
	field(i)%element=num
	field(i)%elem=num*2
	num=num-1
end do
mask2 = reshape(mask1, (/2,4/))
res = pack(vec,mask2,field)
!!print loop
print *, "element"
do i = 1,8
		print *, res(i)%element
end do
print *, "elem"
do i = 1,8
		print *, res(i)%elem
end do

end program a
