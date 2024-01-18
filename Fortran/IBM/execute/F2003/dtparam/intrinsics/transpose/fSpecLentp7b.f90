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
!       Transpose Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Description: uses external functions for matrix multiplication, 
!*                               with an interfaced operator.
!*
!*
!*
!*
module m1

!derived type definitions

type adrow(k)
  integer, kind :: k
  integer(k) :: element 
end type adrow
 
type admatrix (k,l1)
  integer, kind :: k
  integer, len :: l1
  type (adrow(k)) :: row(l1)
end type admatrix

!interfaces for matrix addition and multiplication

	interface operator(*)
		module procedure matrix_scalarmulti, matrix_multi
	end interface 	
	
	interface operator(+)
		module procedure matrix_addition
	end interface
	
contains

!subroutine to check equality of matrices
	subroutine equality_check(matrixA, matrixB)
		type(admatrix(4,l1=*)), intent(in) :: matrixA(:,:)
		type(admatrix(4,l1=*)), intent(in) :: matrixB(:,:)
		common err_check
		integer err_check
		err_check = err_check + 1
		if (ubound(matrixA,1) /= ubound(matrixB,1) .or. ubound(matrixA,2) /= ubound(matrixB,2)) STOP 1
		do i=1, ubound(matrixA,2)
			do j=1, ubound(matrixA,1)
				do k=1,matrixA%l1
					if (matrixA(j,i)%row(k)%element /= matrixB(j,i)%row(k)%element) then
						print *, "error occured in number", err_check, "call to equality_check" 
						STOP 2
					end if
				end do
			end do
		end do
	end subroutine equality_check

!function to add matrices	
	function matrix_addition(matrixA, matrixB) result (X)
		type(admatrix(4,l1=*)), intent(in) :: matrixA(:,:)
		type(admatrix(4,l1=*)), intent(in) :: matrixB(:,:)
		type(admatrix(4,l1=matrixA%l1)) :: X(ubound(matrixA,1), ubound(matrixA,2))
		
		do i=1, ubound(matrixA,2)
			do j=1,ubound(matrixA,1)
				do k=1,matrixA%l1
					X(j,i)%row(k)%element = matrixA(j,i)%row(k)%element + matrixB(j,i)%row(k)%element
				end do
			end do
		end do
	end function matrix_addition	

!function to multiply matrices	
	function matrix_multi(matrixA, matrixB) result(X)
		type(admatrix(4,l1=*)), intent(in) :: matrixA(:,:)
		type(admatrix(4,l1=*)), intent(in) :: matrixB(:,:)
		type(admatrix(4,l1=matrixA%l1)) :: X(ubound(matrixA,1),ubound(matrixB,2))
			
		do i=1, ubound(matrixB,2)
			do j=1,ubound(matrixA,1)
				do m=1,matrixA%l1
					X(j,i)%row(m)%element=0
				end do
			end do
			do k = 1,ubound(matrixA,2)
				do j=1,ubound(matrixA,1)
					do m=1,matrixA%l1
						X(j,i)%row(m)%element = X(j,i)%row(m)%element + matrixA(j,k)%row(m)%element*matrixB(k,i)%row(m)%element
					end do
				end do
			end do 
		end do
  	end function matrix_multi

!function to multiply scalars and matrices
	function matrix_scalarmulti(matrixA, constant) result (X)
		type(admatrix(4,l1=*)), intent(in) :: matrixA(:,:)
		integer, intent(in) :: constant
		type(admatrix(4,l1=matrixA%l1)) :: X(ubound(matrixA,1), ubound(matrixA,2))
		
		do i = 1, ubound(matrixA,2)
			do j = 1, ubound(matrixA,1)
				do k=1,matrixA%l1
					X(j,i)%row(k)%element = constant * matrixA(j,i)%row(k)%element
				end do	
			end do
		end do
	end function matrix_scalarmulti

!function to perform the transpose intrinsic function 
	function transposer(matrixA,length) result(X)
		type(admatrix(4,l1=length)), intent(in)::matrixA(:,:)
		type(admatrix(4,l1=length)) :: X(ubound(matrixA,2), ubound(matrixA,1))
		X = transpose(matrixA)
	end function transposer	
	
end module m1

program a	

!Test transpose primarily through mathematical properties

use m1
type (admatrix(4,:)),allocatable:: matrix1(:,:), matrix2(:,:)
type(admatrix(4,3)) :: matrix1t(20,10)
!type (admatrix(4,3)) :: matrix1(10,20), matrix2(10,20), matrix1t(20,10)
integer :: constant, num, length
common err_check
integer err_check
err_check=0

length = 3
allocate(admatrix(4,length)::matrix1(10,20), matrix2(10,20))
!allocate(admatrix(4,3)::matrix1(10,20), matrix2(10,20))

!Array initialization
	num = 1
	do k =1,ubound(matrix1,1)
		do j = 1,ubound(matrix1,2)
			do i=1,matrix1%l1
				matrix1(k,j)%row(i)%element = num
			end do 
			num = num + 1
		end do
	end do
	num = 1
	do k =1,ubound(matrix2,1)
		do j = 1,ubound(matrix2,2)
			do i=1,matrix2%l1
				matrix2(k,j)%row(i)%element = num
			end do
			num = num + 2
		end do
	end do
	
	!! Att = A
	
	matrix1t = transpose(matrix1)
	call equality_check(matrix1, transpose(matrix1t))
	call equality_check(matrix1, transpose(transpose(matrix1)))
	call equality_check(matrix2, transpose(transpose(matrix2)))
	
	!!At + Bt =(A+B)t
	call equality_check(transpose(matrix1 + matrix2), transpose(matrix1) + transpose(matrix2))
		
	!!(AB)t = BtAt
	call equality_check(transpose(matrix1*transpose(matrix2)), matrix2*transpose(matrix1))
	call equality_check(transpose(matrix2*transpose(matrix1)), matrix1*transpose(matrix2))
	
	!!(cAt) = cAt
	constant = 10
	matrix2 = matrix1*constant
	call equality_check(transpose(matrix1*constant), transpose(matrix1)*constant)
	call equality_check(transpose(matrix2*constant), transpose(matrix2)*constant)
end program a