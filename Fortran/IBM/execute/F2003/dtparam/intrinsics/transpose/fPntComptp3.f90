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
!*                               with an interfaced operator.  DTP contains pointer component.
!*
!*
!*
!*
module m1
type adrow(k)
  integer, kind :: k
  integer(k), pointer::element=>null()
end type adrow
 
type admatrix (k1)
  integer, kind :: k1
  type (adrow(k1)) :: row
end type admatrix

	interface operator(*)
		module procedure matrix_multi, matrix_scalarmulti
	end interface 	
	
	interface operator(+)
		module procedure matrix_addition
	end interface
	
contains
	
		subroutine equality_check(matrixA, matrixB)
			type(admatrix(4)), intent(in) :: matrixA(:,:)
			type(admatrix(4)), intent(in) :: matrixB(:,:)
			integer err_check
			common err_check
			err_check = err_check + 1
			if (ubound(matrixA,1) /= ubound(matrixB,1) .or. ubound(matrixA,2) /= ubound(matrixB,2)) STOP 1
			do i=1, ubound(matrixA,2)
				do j=1, ubound(matrixA,1)
				if (matrixA(j,i)%row%element /= matrixB(j,i)%row%element) then
						print *, "error occured in number", err_check, "call to equality_check" 
						STOP 2
				end if
				end do
			end do
		end subroutine equality_check

		function matrix_addition(matrixA, matrixB) result (X)
			type(admatrix(4)), intent(in) :: matrixA(:,:)
			type(admatrix(4)), intent(in) :: matrixB(:,:)
			type(admatrix(4)) :: X(ubound(matrixA,1), ubound(matrixA,2))
			
			do i=1, ubound(matrixA,2)
				do j=1,ubound(matrixA,1)
					allocate(X(j,i)%row%element, SOURCE = matrixA(j,i)%row%element + matrixB(j,i)%row%element)
				end do
			end do
		end function matrix_addition	
		
		function matrix_multi(matrixA, matrixB) result(X)
			type(admatrix(4)), intent(in) :: matrixA(:,:)
			type(admatrix(4)), intent(in) :: matrixB(:,:)
			type(admatrix(4)) :: X(ubound(matrixA,1),ubound(matrixB,2))
			
			do i=1, ubound(matrixB,2)
				do j=1,ubound(matrixA,1)
					allocate(X(j,i)%row%element, SOURCE=0)
				end do
				do k = 1,ubound(matrixA,2)
					do j=1,ubound(matrixA,1)
						allocate(X(j,i)%row%element, SOURCE = X(j,i)%row%element + matrixA(j,k)%row%element * matrixB(k,i)%row%element)
					end do
				end do 
			end do
	  	end function matrix_multi
		
		function matrix_scalarmulti(matrixA, constant) result (X)
			type(admatrix(4)), intent(in) :: matrixA(:,:)
			integer, intent(in) :: constant
			type(admatrix(4)) :: X(ubound(matrixA,1), ubound(matrixA,2))
			
			do i = 1, ubound(matrixA,2)
				do j = 1, ubound(matrixA,1)
					allocate(X(j,i)%row%element, SOURCE = constant * matrixA(j,i)%row%element)
				end do
			end do
		end function matrix_scalarmulti
end module m1

program a	

!Test transpose primarily through mathematical properties

use m1
type (admatrix(4)) :: matrix1(10,20), matrix2(10,20), matrix1t(20,10), matrix2t(20,10), matrix3t(20,10)
type (admatrix(4)) :: add1(20,10), add2(20,10), mult1(10,10), mult2(10,10), mult1t(10,10)
integer :: constant, num
integer err_check
common err_check
err_check = 0

	num = 1
	do k =1,ubound(matrix1,1)
		do j = 1,ubound(matrix1,2)
			allocate(matrix1(k,j)%row%element, SOURCE = num)
			num = num + 1
		end do
	end do
	num = 1
	do k =1,ubound(matrix2,1)
		do j = 1,ubound(matrix2,2)
			allocate(matrix2(k,j)%row%element, SOURCE = num)
			num = num + 2
		end do
	end do
	
	! Att = A
	matrix1t = transpose(matrix1)
	matrix2 = transpose(matrix1t)
	call equality_check(matrix1, matrix2)
	
	!At + Bt =(A+B)t
	matrix1t = transpose(matrix1)
	matrix2t = transpose(matrix2)
	 add1 = transpose( matrix1 + matrix2)
	 add2 = matrix1t + matrix2t
	 call equality_check(add1, add2)
	
	!(AB)t = BtAt
	matrix1t = transpose(matrix1)
	matrix2t = transpose(matrix2)
	mult1=matrix1*matrix2t !AB
	mult2=matrix2*matrix1t !BtAt
	mult1t = transpose(mult1) !(AB)t
	call equality_check(mult2, mult1t)
	
	!(cAt) = cAt
	 constant = 10
	 matrix1t = transpose(matrix1)
	 matrix3t = matrix1t*constant
	 matrix2t = transpose(matrix1*constant)
	 call equality_check(matrix2t, matrix3t)

end program a