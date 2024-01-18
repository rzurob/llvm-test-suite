!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : UNPACK DTP INTRINSIC FUNCTION
!*
!*  PROGRAMMER                 : Adrian Green
!*  DATE                       : July 27, 2008
!*  ORIGIN                     : XLF Compiler Test,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Pack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Uses pack with a derived type component of a derived type 
!*								 containing a pointer and a real component. UNPACK is placed   
!*								in a seperate subroutine, in a seperate module and interfaced.
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
		module procedure unpacked
	end interface 	
	
contains
	function unpacked(dtp, mask) result (X)
			type(admatrix(4)), intent(in), dimension(:) :: dtp
			logical, intent(in), dimension(:,:) :: mask
			type(admatrix(4)), dimension(8):: field1
			type(admatrix(4)), dimension(2,4) :: X
			type(admatrix(4)), dimension(2,4) :: field2
			do j = 1,8
				do i = 1,3
					allocate (field1(j)%row(i)%element, SOURCE = -1.0)
				end do
			end do
			field2 = reshape(field1, (/2, 4/))
			X = unpack(dtp, mask, field2)
			!allocate(X, SOURCE=unpack(dtp, mask, field2))
		end function unpacked
		
end module m1

program a	

use m1
type (admatrix(4)), allocatable :: vec(:) !vec(4)
type (admatrix(4)), allocatable :: field1(:)  !field1(8)
type (admatrix(4)), allocatable :: res(:,:), field2(:,:)  !res(2,4), field2(2,4)
logical :: mask1(8) = (/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE./)
logical :: mask2(2,4)
real :: num
real :: res2(6)
allocate( admatrix(4) :: vec(4), field1(8), field2(2,4))
num = 1.0
do j = 1,4
	do i = 1,3
		allocate(vec(j)%row(i)%element, SOURCE = num)
		num = num + 1.0
	end do 
end do
mask2 = reshape(mask1, (/2, 4/))
allocate(res(ubound(vec+mask2,1),ubound(vec+mask2,2)) &
	, SOURCE=vec+mask2)
!!UNPACK call through interface
!!print loop
do k =1,2
	do i = 1,4
		print *, res(k,i)%row(1)%element, '  ', res(k,i)%row(2)%element, '  ', res(k,i)%row(3)%element
	end do
end do
end program a