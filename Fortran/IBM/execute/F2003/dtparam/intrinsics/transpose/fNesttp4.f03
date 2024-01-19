!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Transpose Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Description: uses external functions for matrix multiplication,
!*                               with an interfaced operator.  Derived type contains integer
!*                               and real components.
!*
module m1
type adrow(k)
  integer, kind :: k
  real(k) :: elem
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
					X(j,i)%row%elem = matrixA(j,i)%row%elem + matrixB(j,i)%row%elem
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
					X(j,i)%row%elem=0.0
				end do
				do k = 1,ubound(matrixA,2)
					do j=1,ubound(matrixA,1)
						X(j,i)%row%element = X(j,i)%row%element + matrixA(j,k)%row%element * matrixB(k,i)%row%element
						X(j,i)%row%elem = X(j,i)%row%elem + matrixA(j,k)%row%elem * matrixB(k,i)%row%elem
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
					X(j,i)%row%elem = constant * matrixA(j,i)%row%elem
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
real :: num2
integer err_check
common err_check
err_check = 0

	num2=10.0
	num = 1
	do k =1,ubound(matrix1,1)
		do j = 1,ubound(matrix1,2)
			matrix1(k,j)%row%element = num
			matrix1(k,j)%row%elem = num2
			nim2 = num2 + 10.0
			num = num + 1
		end do
	end do
	num2 = 10.0
	num = 1
	do k =1,ubound(matrix2,1)
		do j = 1,ubound(matrix2,2)
			matrix2(k,j)%row%element = num
			matrix2(k,j)%row%elem = num
			num = num + 2
			num2=num2+20.0
		end do
	end do

	!! Att = A
	matrix1t = transpose(matrix1)
	matrix2 = transpose(matrix1t)
	call equality_check(matrix1, matrix2)

	!!At + Bt =(A+B)t
	matrix1t = transpose(matrix1)
	matrix2t = transpose(matrix2)
	add1 = transpose( matrix1 + matrix2)
	add2 = matrix1t + matrix2t
	call equality_check(add1, add2)

	!!(AB)t = BtAt
	matrix1t = transpose(matrix1)
	matrix2t = transpose(matrix2)
	mult1=matrix1*matrix2t !AB
	mult2=matrix2*matrix1t !BtAt
	mult1t = transpose(mult1) !(AB)t
	call equality_check(mult2, mult1t)

	!!(cAt) = cAt
	constant = 10
	matrix1t = transpose(matrix1)
	matrix3t = matrix1t*constant
	matrix2t = transpose(matrix1*constant)
	call equality_check(matrix2t, matrix3t)

end program a