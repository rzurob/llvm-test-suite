!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Pack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Uses pack with a derived type component with a derived type component
!*								 containing a derived type component. UNPACK is placed
!*								in a seperate subroutine, in a seperate module and interfaced.
!*
module m1
	type adelem(k)
		integer, kind :: k
		real(k) :: elem
	end type adelem

	type adrow(k1)
		integer, kind :: k1
		real(k1) :: element
		type (adelem(k1)) :: elema
	end type adrow

	type admatrix (k2)
		integer, kind :: k2
		type (adrow(k2)), dimension(3) :: row
	end type admatrix
end module m1

module m2
	use m1
	interface operator(+)
		module procedure unpacked
	end interface
contains
	function unpacked(dtp, mask) result (X)
			type(admatrix(4)), intent(in), dimension(:) :: dtp
			logical, intent(in), dimension(:,:) :: mask
			type(admatrix(4)), dimension(8):: field1
			type(admatrix(4)), dimension(4,2) :: X
			type(admatrix(4)), dimension(4,2) :: field2
			do j = 1,8
				do i = 1,3
					field1(j)%row(i)%elema%elem = -1.0
					field1(j)%row(i)%element = -1.0
				end do
			end do
			field2 = reshape(field1, (/4, 2/))
			X = unpack(dtp, mask, field2)
	end function unpacked
end module m2

program a

use m1
use m2
type (admatrix(4)) :: vec(4)
type (admatrix(4)) :: field1(8)
type (admatrix(4)) :: res(4,2), field2(4,2)
logical :: mask1(8) = (/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .FALSE., .TRUE./)
logical :: mask2(4,2)
real :: num

num = 1.0
do j = 1,4
	do i = 1,3
	vec(j)%row(i)%elema%elem = num
	vec(j)%row(i)%element = num*2
	num = num + 1.0
	end do
end do
mask2 = reshape(mask1, (/4, 2/))
field2 = reshape(field1, (/4, 2/))
!!UNPACK call through interface
res = vec + mask2
!!print loop
print *, "elem"
do k =1,4
	do j=1,2
		print *, res(k,j)%row(1)%elema%elem, '  ', res(k,j)%row(2)%elema%elem, '  ',res(k,j)%row(3)%elema%elem
	end do
	print *, ' '
end do
print *, "element"
do k =1,4
	do j=1,2
		print *, res(k,j)%row(1)%element, '  ', res(k,j)%row(2)%element, '  ',res(k,j)%row(3)%element
	end do
	print *, ' '
end do
end program a