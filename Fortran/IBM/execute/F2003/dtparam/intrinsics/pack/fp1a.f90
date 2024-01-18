!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*  ORIGIN                     : XLF Compiler Test,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Pack Intrinsic function with derived type parameters.
!*  DESCRIPTION                :
!*
module m1
type adrow(k)
  integer, kind :: k
  real(k), pointer :: element => null()
end type adrow

type admatrix (k1)
  integer, kind :: k1
  type (adrow(k1)), dimension(3) :: row
end type admatrix

	interface operator(*)
		module procedure vector_multi, arrayvector_multi
	end interface

contains

		function vector_multi(matrixA, matrixB) result (X)
			type(admatrix(4)), intent(in) :: matrixA
			type(admatrix(4)), intent(in) :: matrixB
			real :: X, dotprod
			dotprod = 0.0
			do i = 1,3
				dotprod = dotprod + matrixA%row(i)%element*matrixB%row(i)%element
			end do
			X = dotprod
		end function vector_multi

		function arrayvector_multi(matrixA, matrixB) result (X)
			type(admatrix(4)), intent(in), dimension(2,3) :: matrixA
			type(admatrix(4)), intent(in), dimension(2,3) :: matrixB
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
end module m1

program a

use m1
type (admatrix(4)) :: res(4)
type (admatrix(4)), allocatable :: vec(:,:), field2(:)
logical :: mask1(6) = (/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE./)
logical :: mask2(2,3)
real :: num
real :: res2(6)
allocate (admatrix(4) :: vec(2,3))
allocate (admatrix(4) :: field2(4))
do i =1,4
	do j= 1,3
		allocate( field2(i)%row(j)%element, SOURCE = 1.0)
	end do
end do

	num = 1.0
	do k =1,2
	do j = 1,3
		do i = 1,3
			allocate(vec(k,j)%row(i)%element, SOURCE = num)
			num = num + 1.0
		end do
	end do
	mask2 = reshape(mask1, (/2, 3/))
	res = pack(vec, mask2, field2)
	!print *, res*res
	!!printing loop
	!!print *, 'res2: ', res2
	do i = 1,4
		!do j = 1,3
			print *, res(i)%row(1)%element, '  ', res(i)%row(2)%element, '  ', res(i)%row(3)%element, '  '
		!end do
	end do
end program a
