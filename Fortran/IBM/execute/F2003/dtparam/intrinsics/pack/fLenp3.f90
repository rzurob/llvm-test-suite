!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*  ORIGIN                     : XLF Compiler Test,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Pack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Description: uses external functions for matrix multiplication,
!*                               with an interfaced operator.
!*
module m1
type adrow(k)
  integer, kind :: k
  real(k), pointer :: element => null()
end type adrow

type admatrix (k1,l1)
  integer, kind :: k1
  integer, len :: l1
  type (adrow(k1)), dimension(l1) :: row
end type admatrix

	interface operator(*)
		module procedure vector_multi, arrayvector_multi
	end interface

contains

		function vector_multi(matrixA, matrixB) result (X)
			type(admatrix(4,3)), intent(in) :: matrixA
			type(admatrix(4,3)), intent(in) :: matrixB
			real :: X, dotprod
			dotprod = 0.0
			do i = 1,3
				dotprod = dotprod + matrixA%row(i)%element*matrixB%row(i)%element
			end do
			X = dotprod
		end function vector_multi

		function arrayvector_multi(matrixA, matrixB) result (X)
			type(admatrix(4,3)), intent(in), dimension(3) :: matrixA
			type(admatrix(4,3)), intent(in), dimension(3) :: matrixB
			real :: X(3)
			do i = 1,3
				X(i) = matrixA(i)* matrixB(i)
			end do
		end function arrayvector_multi
end module m1

program a

use m1
type (admatrix(4,3)) :: res(3)
type (admatrix(4,3)) :: field1(6)
type (admatrix(4,3)) :: vec(2,3), field2(2,3)
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
	mask2 = reshape(mask1, (/2, 3/))
	field2 = reshape(field1, (/2, 3/))
	res = pack(vec, mask2)
	print *, res * res
	do i = 1,3
		print *, res(i)%row(1)%element, '  ', res(i)%row(2)%element, '  ', res(i)%row(3)%element, '  '
	end do
end program a