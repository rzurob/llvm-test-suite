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
!*  DESCRIPTION                : Uses pack with a derived type component with a derived type component 
!*								 containing a derived type component. PACK is placed   
!*								in a seperate subroutine, in a seperate module and interfaced.
!*
!*
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
contains
	function packed(dtp, mask) result (X)
			type(admatrix(4)), intent(in), dimension(:,:) :: dtp
			logical, intent(in), dimension(:,:) :: mask
			type(admatrix(4)), dimension(8) :: field1
			type(admatrix(4)), dimension(8) :: X
			do j = 1,ubound(field1,1)
				do i = 1,3
					field1(j)%row(i)%element = -1.0
					field1(j)%row(i)%elema%elem = -1.0
				end do
			end do
			X = pack(dtp, mask, field1)
			!allocate(X, SOURCE=unpack(dtp, mask, field2))
		end function packed
end module m2

program a	

use m1
use m2
type (admatrix(4)), pointer :: vec(:,:), res(:)  !vec(2,4), res(8)
logical :: mask1(8) = (/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE./)
logical :: mask2(2,4)
real :: num
real :: res2(6)

allocate( admatrix(4) :: vec(2,4), res(8))

num = 1.0
do k = 1,2
	do j = 1,4
		do i = 1,3
			vec(k,j)%row(i)%element = num
			vec(k,j)%row(i)%elema%elem = num
			num = num + 1.0
		end do 
	end do
end do
mask2 = reshape(mask1, (/2,4/))
res = packed(vec, mask2)
!!print loop
print *, "element"
do i = 1,8
		print *, res(i)%row(1)%element, '  ', res(i)%row(2)%element, '  ', res(i)%row(3)%element
end do
print *, "elem"
do i = 1,8
		print *, res(i)%row(1)%elema%elem, '  ', res(i)%row(2)%elema%elem, '  ', res(i)%row(3)%elema%elem
end do

end program a
