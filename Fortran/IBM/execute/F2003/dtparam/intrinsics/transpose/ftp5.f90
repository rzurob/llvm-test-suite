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
type adrow(k)
  integer, kind :: k
  integer(k) :: element 
end type adrow
 
type admatrix (k1,l1,l2)
  integer, kind :: k1
  integer, len :: l1
  integer, len :: l2
  type (adrow(k1)) :: row(l1,l2)
end type admatrix

	interface operator(*)
		module procedure matrix_scalarmulti, matrix_multi
	end interface 	
	
	interface operator(+)
		module procedure matrix_addition
	end interface
	
contains
	
		subroutine equality_check(matrixA, matrixB)
			type(adrow(4)), intent(in) :: matrixA(:,:)
			type(adrow(4)), intent(in) :: matrixB(:,:)
			integer err_check
			common err_check
			err_check = err_check + 1
			if (ubound(matrixA,1) /= ubound(matrixB,1) .or. ubound(matrixA,2) /= ubound(matrixB,2)) STOP 1
			do i=1, ubound(matrixA,2)
				do j=1, ubound(matrixA,1)
				if (matrixA(j,i)%element /= matrixB(j,i)%element) then
					print *, "error occured in number", err_check, "call to equality_check" 
					STOP 2
				end if
				end do
			end do
		end subroutine equality_check

		function matrix_addition(matrixA, matrixB) result (X)
			type(adrow(4)), intent(in) :: matrixA(:,:)
			type(adrow(4)), intent(in) :: matrixB(:,:)
			type(adrow(4)) :: X(ubound(matrixA,1),ubound(matrixA,2))
			
			do i=1, ubound(matrixA,2)
				do j=1,ubound(matrixA,1)
					X(j,i)%element = matrixA(j,i)%element + matrixB(j,i)%element
				end do
			end do
		end function matrix_addition	
		
		
		function matrix_multi(matrixA, matrixB) result(X)
			type(adrow(4)), intent(in) :: matrixA(:,:)
			type(adrow(4)), intent(in) :: matrixB(:,:)
			type(adrow(4)) :: X(ubound(matrixA,1),ubound(matrixB,2))
			
			do i=1, ubound(matrixB,2)
				do j=1,ubound(matrixA,1)
					X(j,i)%element=0
				end do
				do k = 1,ubound(matrixA,2)
					do j=1,ubound(matrixA,1)
						X(j,i)%element = X(j,i)%element + matrixA(j,k)%element*matrixB(k,i)%element
					end do
				end do 
			end do
	  	end function matrix_multi

!function to multiply scalars and matrices
	function matrix_scalarmulti(matrixA, constant) result (X)
		type(adrow(4)), intent(in) :: matrixA(:,:)
		integer, intent(in) :: constant
		type(adrow(4)) :: X(ubound(matrixA,1),ubound(matrixA,2)) 
		
		do i = 1, ubound(matrixA,2)
			do j = 1,ubound(matrixA,1)
				X(j,i)%element = constant * matrixA(j,i)%element
			end do
		end do
	end function matrix_scalarmulti
end module m1

program a	

!Test transpose primarily through mathematical properties

use m1
type (admatrix(4,10,20)) :: matrix1, matrix2 
type (admatrix(4,20,10)) :: matrix1t, matrix2t, matrix3t, add1, add2
type (admatrix(4,10,10)) :: mult1, mult2, mult1t
integer :: constant, num
integer err_check
common err_check
err_check = 0

	num = 1
	do k =1,matrix1%l1
		do j = 1,matrix1%l2
			matrix1%row(k,j)%element = num
			num = num + 1
		end do
	end do
		
	! Att = A
	matrix1t%row = transpose(matrix1%row)
	matrix2%row = transpose(matrix1t%row)
	call equality_check(matrix1%row, matrix2%row)
	
	!At + Bt =(A+B)t
	matrix1t%row = transpose(matrix1%row)
	matrix2t%row = transpose(matrix2%row)
	add1%row = transpose(matrix1%row + matrix2%row)
	add2%row = matrix1t%row + matrix2t%row
	call equality_check(add1%row, add2%row)
	
	
	!!(AB)t = BtAt
	matrix1t%row = transpose(matrix1%row)
	matrix2t%row = transpose(matrix2%row)
	mult1%row = matrix1%row*matrix2t%row !AB
	mult2%row = matrix2%row*matrix1t%row !BtAt
	mult1t%row = transpose(mult1%row) !(AB)t
	call equality_check(mult2%row, mult1t%row)
	
	!(cA)t = c(At)
	constant = 10
	matrix1t%row = transpose(matrix1%row)
	matrix3t%row = matrix1t%row * constant
	matrix1t%row = matrix1t%row * constant
	call equality_check(matrix3t%row, matrix1t%row)
	matrix2t%row = transpose(matrix1%row * constant)
	call equality_check(matrix2t%row, matrix3t%row)
	
end program a