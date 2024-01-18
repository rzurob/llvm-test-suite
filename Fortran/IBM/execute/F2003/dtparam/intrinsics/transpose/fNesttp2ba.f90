!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Transpose Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Description: uses external functions for matrix multiplication,
!*                               with an interfaced operator.
!*
module m1
type adrow(k)
  integer, kind :: k
  integer(k) :: element
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
			if (ubound(matrixA,1) /= ubound(matrixB,1) .or. ubound(matrixA,2) /= ubound(matrixB,2)) ERROR STOP 1
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
					X(j,i)%row%element = matrixA(j,i)%row%element + matrixB(j,i)%row%element
				end do
			end do
		end function matrix_addition

		function matrix_multi(matrixA, matrixB) result(X)
			type(admatrix(4)), intent(in) :: matrixA(:,:)
			type(admatrix(4)), intent(in) :: matrixB(:,:)
			type(admatrix(4)) :: X(ubound(matrixA,1),ubound(matrixB,2))

			do i=1, ubound(matrixB,2)
				do j=1,ubound(matrixA,1)
					X(j,i)%row%element=0
				end do
				do k = 1,ubound(matrixA,2)
					do j=1,ubound(matrixA,1)
						X(j,i)%row%element = X(j,i)%row%element + matrixA(j,k)%row%element * matrixB(k,i)%row%element
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
					X(j,i)%row%element = constant * matrixA(j,i)%row%element
				end do
			end do
		end function matrix_scalarmulti

end module m1

program a

!Test transpose primarily through mathematical properties

use m1
type (admatrix(4)), allocatable :: matrix1(:,:), matrix2(:,:) !matrix1(10,20), matrix2(10,20),
integer constant, num
integer err_check
common err_check
err_check = 0

allocate(admatrix(4) :: matrix1(10,20))
	num = 1
	do k =1,ubound(matrix1,1)
		do j = 1,ubound(matrix1,2)
			matrix1(k,j)%row%element = num
			num = num + 1
		end do
	end do
allocate(admatrix(4) :: matrix2(10,20))
	num = 1
	do k =1,ubound(matrix2,1)
		do j = 1,ubound(matrix2,2)
			matrix2(k,j)%row%element = num
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