!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Pack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : PACK uses pack with a derived type component with a deffered length parameter
!*								Derived type has real and integer array components
!*
module m1
	type dtp(l)
		integer, len :: l
		real :: com
		integer :: comp(l)
	end type dtp
end module m1

program a
use m1
type (dtp(:)), pointer :: point(:,:)
type (dtp(3)), dimension(4,4) :: matrixA
type (dtp(3)), dimension(8) :: vec, res
logical :: mask1(4,4)
integer :: num
num = 1
do i = 1,4
	do j = 1,4
		do k = 1,3
			matrixA(j,i)%comp(k) = num
			num = num + 1
		end do
		matrixA(j,i)%com = 1.0
		num = i
	end do
	num = i
end do

allocate(point(lbound(matrixA,1):ubound(matrixA,1), lbound(matrixA,2):ubound(matrixA,2)), source = matrixA)

do i = 1,4
	do j = 1,4
		mask1(j,i) = .FALSE.
		if ( i==j) mask1(j,i) = .TRUE.
	end do
end do
do i = 1,8
	do j = 1,3
		vec(i)%comp(j) = 1
	end do
	vec(i)%com = 1.0
end do

res = pack (point, mask1, vec)

print *, "comp"
do i = 1,8
	print *, res(i)%comp(1), '  ', res(i)%comp(2), '  ', res(i)%comp(3)
end do
print *, "com"
do i = 1,8
	print *, res(i)%com
end do
end program a