!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            :  UNPACK DTP INTRINSIC FUNCTION
!*
!*  PROGRAMMER                 : Adrian Green
!*  DATE                       : July 27, 2008
!*  ORIGIN                     : XLF Compiler Test,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Pack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Description: uses external functions for matrix multiplication, 
!*                               with an interfaced operator.
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
		type(admatrix(4,l=*)), intent(in), dimension(:,:) :: matrixA
		type(admatrix(4,l=*)), intent(in), dimension(:,:) :: matrixB
		real :: X(6)
		integer j,k
		j = 1
		k=1
		do i = 1,6
			j=i
			if(i > 3) then 
				j = i - 3
				k = 2
			end if
			X(i) = matrixA(k,j)*matrixB(k,j)
		end do	
	end function arrayvector_multi
		
		function unpacker(A,field,M) result (X)
			type (admatrix(4,l=*)), intent(in), dimension(:) :: A
			type (admatrix(4,l=*)), intent(in), dimension(:,:) :: field
			type (admatrix(4,:)), dimension(:,:), allocatable :: X
			logical, intent(in), dimension(:,:) :: M
			allocate (X(UBOUND(unpack(A,M,field),1),UBOUND(unpack(A,M,field),2)), SOURCE = unpack(A,M,field))
		end function unpacker
		
end module m1

program a	

use m1
type (admatrix(4,3)) :: res(2,3) 
type (admatrix(4,3)) :: field(2,3) 
type (admatrix(4,3)) :: vec(3) 
logical :: mask1(6) = (/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE./)
logical :: mask2(2,3)
real :: num
real :: res2(2,3)
	num = 1.0
	do j = 1,3
		do i = 1,3
			allocate(vec(j)%row(i)%element, SOURCE = num)
			num = num + 1.0
		end do 
	end do
	do k =1,2
	do i = 1,3
		do j =1,3
			allocate(field(k,i)%row(j)%element, SOURCE = -1.0)
		end do
	end do
	end do
	mask2 = reshape(mask1, (/2, 3/))
res=unpacker(vec,field,mask2)
!print array of dot products from vector res	
	print *, res * res
	do j=1,2
		do i = 1,3
		print *, res(j,i)%row(1)%element, '  ', res(j,i)%row(2)%element, '  ', res(j,i)%row(3)%element, '  '
		end do
		print *, ' ' 
	end do
end program a