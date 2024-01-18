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
!*  DESCRIPTION                : Description: uses external functions for matrix multiplication, 
!*                               with an interfaced operator.  pack performed on assumed length dtp.
!*
!*
!*
!*
module m1
type adrow(k)
  integer, kind :: k
  real(k), pointer :: element => null()
end type adrow
 
type admatrix (k1,l)
  integer, kind :: k1
  integer, len :: l
  type (adrow(k1)), dimension(l) :: row
end type admatrix

	interface operator(*)
		module procedure vector_multi, arrayvector_multi
	end interface 	
	
	! 
contains

		function vector_multi(matrixA, matrixB) result (X)
			type(admatrix(4,l=*)), intent(in) :: matrixA
			type(admatrix(4,l=*)), intent(in) :: matrixB
			real :: X, dotprod
			dotprod = 0.0
			do i = 1,3	
				dotprod = dotprod + matrixA%row(i)%element*matrixB%row(i)%element
			end do 
			X = dotprod
		end function vector_multi

		function arrayvector_multi(matrixA, matrixB) result (X)
			type(admatrix(4,l=*)), intent(in), dimension(3) :: matrixA
			type(admatrix(4,l=*)), intent(in), dimension(3) :: matrixB
			real :: X(3)
			do i = 1,3
				X(i) = matrixA(i)* matrixB(i)
			end do	
		end function arrayvector_multi
		
		function packer(A,field,M) result (X)
			type (admatrix(4,l=*)), intent(in), dimension(:,:) :: A
			type (admatrix(4,l=*)), intent(in), dimension(:) :: field
			type (admatrix(4,:)), dimension(:), allocatable :: X
			logical, intent(in), dimension(:,:) :: M
			allocate (X(UBOUND(pack(A,M,field),1)), SOURCE = pack(A,M,field))
		end function packer
		
end module m1

program a	

use m1
type (admatrix(4,3)) :: res(3)
type (admatrix(4,3)) :: field(6)
type (admatrix(4,3)) :: vec(2,3)
logical :: mask1(6) = (/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE./)
logical :: mask2(2,3)
real :: num
real :: res2(6)

	num = 1.0
	do k =1,2
	do j = 1,3
		do i = 1,3
			allocate(vec(k,j)%row(i)%element, SOURCE = num)
			num = num + 1.0
		end do 
	end do
	end do
	do i = 1,6
		do j =1,3
			allocate(field(i)%row(j)%element, SOURCE = -1.0)
		end do
	end do
	mask2 = reshape(mask1, (/2, 3/))
	res = packer(vec,field,mask2)
	print *, res * res
	do i = 1,3
		print *, res(i)%row(1)%element, '  ', res(i)%row(2)%element, '  ', res(i)%row(3)%element, '  '
	end do
end program a