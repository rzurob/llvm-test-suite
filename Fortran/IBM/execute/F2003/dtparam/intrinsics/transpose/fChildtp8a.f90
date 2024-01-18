!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*  ORIGIN                     : XLF Compiler Test,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Transpose Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Description: uses external functions for matrix multiplication,
!*                               with an interfaced operator.
!*
module m1
type base
    integer :: element
end type base

type, extends(base) :: child
	integer :: elem
end type child

	interface operator(*)
		module procedure matrix_multi, matrix_scalarmulti
	end interface

	interface operator(+)
		module procedure matrix_addition
	end interface

contains

		subroutine equality_check(matrixA, matrixB)
			type(child), intent(in) :: matrixA(:,:)
			type(child), intent(in) :: matrixB(:,:)
			common err_check
			integer err_check
			err_check = err_check + 1
			if (ubound(matrixA,1) /= ubound(matrixB,1) .or. ubound(matrixA,2) /= ubound(matrixB,2)) STOP 1
			do i=1, ubound(matrixA,2)
				do j=1, ubound(matrixA,1)
					if (matrixA(j,i)%element /= matrixB(j,i)%element) then
						print *, "error occured in number", err_check, "call to equality_check"
						STOP 2
					end if
					if (matrixA(j,i)%elem /= matrixB(j,i)%elem) then
						print *, "error occured in number", err_check, "call to equality_check"
						STOP 3
					end if
				end do
			end do
		end subroutine equality_check

		function matrix_addition(matrixA, matrixB) result (X)
			type(child), intent(in) :: matrixA(:,:)
			type(child), intent(in) :: matrixB(:,:)
			type(child) :: X(ubound(matrixA,1), ubound(matrixA,2))

			do i=1, ubound(matrixA,2)
				do j=1,ubound(matrixA,1)
					X(j,i)%element = matrixA(j,i)%element + matrixB(j,i)%element
					X(j,i)%elem = matrixA(j,i)%elem + matrixB(j,i)%elem
				end do
			end do
		end function matrix_addition

		function matrix_multi(matrixA, matrixB) result(X)
			type(child), intent(in) :: matrixA(:,:)
			type(child), intent(in) :: matrixB(:,:)
			type(child) :: X(ubound(matrixA,1),ubound(matrixB,2))

			do i=1, ubound(matrixB,2)
				do j=1,ubound(matrixA,1)
					X(j,i)%element=0
					X(j,i)%elem=0
				end do
				do k = 1,ubound(matrixA,2)
					do j=1,ubound(matrixA,1)
						X(j,i)%element = X(j,i)%element + matrixA(j,k)%element * matrixB(k,i)%element
						X(j,i)%elem = X(j,i)%elem + matrixA(j,k)%elem * matrixB(k,i)%elem
					end do
				end do
			end do
	  	end function matrix_multi

		function matrix_scalarmulti(matrixA, constant) result (X)
			type(child), intent(in) :: matrixA(:,:)
			integer, intent(in) :: constant
			type(child) :: X(ubound(matrixA,1), ubound(matrixA,2))

			do i = 1, ubound(matrixA,2)
				do j = 1, ubound(matrixA,1)
					X(j,i)%element = constant * matrixA(j,i)%element
					X(j,i)%elem = constant * matrixA(j,i)%elem
				end do
			end do
		end function matrix_scalarmulti

end module m1

program a

!Test transpose primarily through mathematical properties

use m1

type(child), allocatable:: matrix1(:,:), matrix2(:,:) ! matrix1(5,10), matrix2(5,10)
integer constant, num
integer err_check
common err_check
err_check = 0

allocate(child :: matrix1(5,10))
	num = 1
	do k =1,ubound(matrix1,1)
		do j = 1,ubound(matrix1,2)
			matrix1(k,j)%element = num
			matrix1(k,j)%elem = num
			num = num + 1
		end do
	end do

allocate(child:: matrix2(5,10))

	num = 1
	do k =1,ubound(matrix2,1)
		do j = 1,ubound(matrix2,2)
			matrix2(k,j)%element = num
			matrix2(k,j)%elem = num
			num = num + 2
		end do
	end do

	!! Att = A
	call equality_check(matrix1, transpose(transpose(matrix1)))
	call equality_check(matrix2, transpose(transpose(matrix2)))

	!!At + Bt =(A+B)t
	call equality_check(transpose(matrix1+matrix2), transpose(matrix1)+transpose(matrix2))

	!!(AB)t = BtAt
	call equality_check(transpose(matrix1*transpose(matrix2)), matrix2*transpose(matrix1))
	call equality_check(transpose(matrix2*transpose(matrix1)), matrix1*transpose(matrix2))

	!!(cAt) = cAt
	constant = 10
	matrix2 = matrix1*constant
	call equality_check(transpose(matrix1*constant), transpose(matrix1)*constant)
	call equality_check(transpose(matrix2*constant), transpose(matrix2)*constant)

end program a