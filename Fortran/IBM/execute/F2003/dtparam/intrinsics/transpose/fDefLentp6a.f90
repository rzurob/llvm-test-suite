!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*  ORIGIN                     : XLF Compiler Test,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Transpose Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Description: uses external functions for matrix multiplication,
!*                               with an interfaced operator. Derived type has allocatable attribute.
!*								length type parameter is deffered.
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
  type (adrow(k)), dimension(l1) :: row
end type admatrix

!interfaces for matrix addition and multiplication

	interface operator(*)
		module procedure matrix_scalarmulti, matrix_multi
	end interface

	interface operator(+)
		module procedure matrix_addition
	end interface

contains

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

end module m1

program a

!Test transpose primarily through mathematical properties

use m1
type (admatrix(4,:)), allocatable :: matrix1(:,:), matrix2(:,:), matrix1t(:,:), matrix2t(:,:), matrix3t(:,:)
!matrix1(10,20), matrix2(10,20), matrix1t(20,10), matrix2t(20,10), matrix3t(20,10)
type (admatrix(4,:)), allocatable :: add1(:,:), add2(:,:), mult1(:,:), mult2(:,:), mult1t(:,:)
!add1(20,10), add2(20,10), mult1(10,10), mult2(10,10), mult1t(10,10)
integer :: constant, num
integer err_check
common err_check
err_check = 0

!Array initialization
allocate(admatrix(4,3) :: matrix1(10,20))
	num = 1
	do k =1,ubound(matrix1,1)
		do j = 1,ubound(matrix1,2)
			do i=1,matrix1%l1
				matrix1(k,j)%row(i)%element = num
			end do
			num = num + 1
		end do
	end do
allocate(admatrix(4,3) :: matrix2(10,20))

	! Att = A
	allocate(matrix1t(ubound(matrix1,2),ubound(matrix1,1)), SOURCE = transpose(matrix1))
	matrix2 = transpose(matrix1t)
	err_check = err_check +1
	do i=1, ubound(matrix1,2)
		do j=1, ubound(matrix1,1)
			do k=1,matrix1%l1
					if (matrix1(j,i)%row(k)%element /= matrix2(j,i)%row(k)%element) then
					print *, "error occured in number", err_check, "call"
					print *, "matrix1(j,i)%row(k)%element /= matrix2(j,i)%row(k)%element,", matrix1(j,i)%row(k)%element, matrix2(j,i)%row(k)%element, "j, i, k", j, i, k
					STOP 2
				end if
			end do
		end do
	end do
	num = 1

	do k =1,ubound(matrix2,1)
		do j = 1,ubound(matrix2,2)
			do i=1,matrix2%l1
				matrix1(k,j)%row(i)%element = num*3
			end do
			num = num + 1
		end do
	end do

	!!At + Bt =(A+B)t
	matrix1t = transpose(matrix1)
	allocate(matrix2t(ubound(matrix2,2),ubound(matrix2,1)), SOURCE = transpose(matrix2))
	allocate(add1(ubound(matrix1+matrix2,2),ubound(matrix1+matrix2,1)),SOURCE=transpose(matrix1+matrix2))
	allocate(add2(ubound(matrix1t+matrix2t,1),ubound(matrix1t+matrix2t,2)),SOURCE=matrix1t+matrix2t)
	err_check = err_check +1
	do i=1, ubound(add1,2)
		do j=1, ubound(add2,1)
			do k=1,add1%l1
					if (add1(j,i)%row(k)%element /= add2(j,i)%row(k)%element) then
					print *, "error occured in number", err_check, "call"
					print *, "add1(j,i)%row(k)%element /= add2(j,i)%row(k)%element, j, i, k", add1(j,i)%row(k)%element, add2(j,i)%row(k)%element, j, i, k
					STOP 2
				end if
			end do
		end do
	end do

	!!(AB)t = BtAt
	!!mult1t is the equivalent of (AB)t
	matrix1t = transpose(matrix1)
	matrix2t = transpose(matrix2)
	allocate(mult1(10,10),SOURCE=matrix1*matrix2t)
	allocate(mult2(10,10),SOURCE=matrix2*matrix1t)
	allocate(mult1t(10,10),SOURCE=transpose(mult1))
	err_check = err_check +1
		do i=1, ubound(mult2,2)
		do j=1, ubound(mult1t,1)
			do k=1,mult2%l1
					if (mult2(j,i)%row(k)%element /= mult1t(j,i)%row(k)%element) then
					print *, "error occured in number", err_check, "call"
					print *, "mult2(j,i)%row(k)%element /= mult1t(j,i)%row(k)%element, j, i, k", mult2(j,i)%row(k)%element, mult1t(j,i)%row(k)%element, j, i, k
					STOP 2
				end if
			end do
		end do
	end do

	!!(cAt) = cAt
	constant = 10
	matrix1t = transpose(matrix1)
	allocate(matrix3t(ubound(matrix1t*constant,1),ubound(matrix1t*constant,2)),SOURCE=matrix1t*constant)
	matrix2t = transpose(matrix1*constant)
	err_check = err_check +1
		do i=1, ubound(matrix2t,2)
		do j=1, ubound(matrix2t,1)
			do k=1,matrix2t%l1
					if (matrix2t(j,i)%row(k)%element /= matrix3t(j,i)%row(k)%element) then
					print *, "error occured in number", err_check, "call"
					print *, "matrix2t(j,i)%row(k)%element /= matrix3t(j,i)%row(k)%element, j, i, k", matrix2t(j,i)%row(k)%element, matrix3t(j,i)%row(k)%element, j, i, k
					STOP 2
				end if
			end do
		end do
	end do

end program a