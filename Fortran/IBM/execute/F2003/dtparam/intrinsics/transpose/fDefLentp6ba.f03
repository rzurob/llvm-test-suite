!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
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

!subroutine to check equality of matrices
	subroutine equality_check(matrixA, matrixB)
		type(admatrix(4,l1=*)), intent(in) :: matrixA(:,:)
		type(admatrix(4,l1=*)), intent(in) :: matrixB(:,:)
		common err_check
		integer err_check
		err_check = err_check + 1
		if (ubound(matrixA,1) /= ubound(matrixB,1) .or. ubound(matrixA,2) /= ubound(matrixB,2))	ERROR STOP 1
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
	call equality_check(matrix1, transpose(transpose(matrix1)))
	allocate(matrix1t(ubound(matrix1,2),ubound(matrix1,1)), SOURCE = transpose(matrix1))
	matrix2 = transpose(matrix1t)
	call equality_check(matrix1,matrix2)

	!!At + Bt =(A+B)t
	matrix1t = transpose(matrix1)
	allocate(matrix2t(ubound(matrix2,2),ubound(matrix2,1)), SOURCE = transpose(matrix2))
	allocate(add1(ubound(matrix1+matrix2,2),ubound(matrix1+matrix2,1)),SOURCE=transpose(matrix1+matrix2))
	allocate(add2(ubound(matrix1t+matrix2t,1),ubound(matrix1t+matrix2t,2)),SOURCE=matrix1t+matrix2t)
	call equality_check(add1, add2)

	!!(AB)t = BtAt
	!!mult1t is the equivalent of (AB)t
	matrix1t = transpose(matrix1)
	matrix2t = transpose(matrix2)
	allocate(mult1(ubound(matrix1*matrix2t,1),ubound(matrix1*matrix2t,2)),SOURCE=matrix1*matrix2t)
	allocate(mult2(ubound(matrix2*matrix1t,1),ubound(matrix2*matrix1t,2)),SOURCE=matrix2*matrix1t)
	allocate(mult1t(ubound(transpose(mult1),1),ubound(transpose(mult1),2)),SOURCE=transpose(mult1))
	call equality_check(mult2, mult1t)

	!!(cAt) = cAt
	constant = 10
	matrix1t = transpose(matrix1)
	allocate(matrix3t(ubound(matrix1t*constant,1),ubound(matrix1t*constant,2)),SOURCE=matrix1t*constant)
	matrix2t = transpose(matrix1*constant)
	call equality_check(matrix2t, matrix3t)

end program a