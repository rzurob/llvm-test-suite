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
!*  DESCRIPTION                : Uses pack with a derived type component of a derived type 
!*								 containing a pointer and a real component. PACK is placed   
!*								in a seperate subroutine, in a seperate module and interfaced.
!*
!*
!*
!*
module m1
type adrow(k)
  integer, kind :: k
  real(k), pointer :: element => null()
end type adrow

type admatrix (k1)
  integer, kind :: k1
  type (adrow(k1)), dimension(3) :: row
end type admatrix

	interface operator(+)
		module procedure packed
	end interface 	
	
contains
	function packed(dtp, mask) result (X)
			type(admatrix(4)), intent(in), dimension(:,:) :: dtp
			logical, intent(in), dimension(:,:) :: mask
			type(admatrix(4)), dimension(10) :: field
			type(admatrix(4)), dimension(10) :: X
			do j = 1,ubound(field,1)
				do i = 1, 3
					allocate (field(j)%row(i)%element, SOURCE = -1.0)
				end do
			end do
			X = pack(dtp, mask, field)
				end function packed
		
end module m1

program a	

use m1
type (admatrix(4)) :: vec(2,4), res(10)
logical :: mask1(8) = (/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE./)
logical :: mask2(2,4)
real :: num

num = 1.0
do k = 1,2
	do j = 1,4
		do i = 1,3
			allocate(vec(k,j)%row(i)%element, SOURCE = num)
			num = num + 1.0
		end do 
	end do
end do
mask2 = reshape(mask1, (/2, 4/))
!!PACK call through interface
!res=vec+mask2
res  = packed(vec, mask2)
!!print loop
do i = 1,4
		print *, res(i)%row(1)%element, '  ', res(i)%row(2)%element, '  ', res(i)%row(3)%element
	end do
end program a