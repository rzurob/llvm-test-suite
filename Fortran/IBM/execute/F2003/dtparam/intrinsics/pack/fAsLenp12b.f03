!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
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

type admatrix (k1,l)
  integer, kind :: k1
  integer, len :: l
  type (adrow(k1)), dimension(l) :: row
end type admatrix

	interface operator(*)
		module procedure vector_multi, arrayvector_multi
	end interface

contains

		function vector_multi(matrixA, matrixB) result (X)
			type(admatrix(4,l=*)), intent(in) :: matrixA
			type(admatrix(4,l=*)), intent(in) :: matrixB
			real :: X, dotprod
			dotprod = 0.0
			do i = 1,3
				dotprod = dotprod + matrixA%row(i)%element*matrixB%row(i)%element
			end do
			X = dotprod
		end function vector_multi

		function arrayvector_multi(matrixA, matrixB) result (X)
			type(admatrix(4,l=*)), intent(in), dimension(3) :: matrixA
			type(admatrix(4,l=*)), intent(in), dimension(3) :: matrixB
			real :: X(3)
			do i = 1,3
				X(i) = matrixA(i)* matrixB(i)
			end do
		end function arrayvector_multi

		function packer(A,M,field,lengA) result (X)
			type (admatrix(4,lengA)), intent(in), dimension(:,:) :: A
			type (admatrix(4,lengA)), intent(in), dimension(:) :: field
			type (admatrix(4,:)), dimension(:), allocatable :: X
			logical, intent(in), dimension(:,:) :: M
			allocate (X(UBOUND(pack(A,M,field),1)), SOURCE = pack(A,M,field))
		end function packer

end module m1

program a

use m1
type (admatrix(4,:)), allocatable :: res(:)!res(3)
type (admatrix(4,:)), allocatable :: field(:) !(4,3)field(6)
type (admatrix(4,:)), allocatable :: vec1(:,:), vec2(:,:) !vec(2,3)
logical :: mask1(6) = (/.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE./)
logical :: mask2(2,3)
integer::lengthA
real :: num
real :: res2(3)
lengthA=3
allocate(admatrix(4,lengthA) :: vec1(2,3))
num = 1.0
	do k =1,2
	do j = 1,3
		do i = 1,lengthA
			allocate(vec1(k,j)%row(i)%element, SOURCE = num)
			num = num + 1.0
		end do
	end do
allocate(vec2(ubound(vec1,1),ubound(vec1,2)),SOURCE=vec1)
allocate(admatrix(4,lengthA)::field(6))
	do i = 1,6
		do j =1,lengthA
			allocate(field(i)%row(j)%element, SOURCE = -1.0)
		end do
	end do
	mask2 = reshape(mask1, (/2, 3/))
allocate(res(ubound(packer(vec2,mask2,field,lengA),1)),SOURCE = packer(vec2,mask2,field,lengA))
res2=res*res
	print *, res2
	do i = 1,3
		print *, res(i)%row(1)%element, '  ', res(i)%row(2)%element, '  ', res(i)%row(3)%element, '  '
	end do
end program a