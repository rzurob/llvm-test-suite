!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*  ORIGIN                     : XLF Compiler Test,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Pack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : PACK uses pack with a derived type component with a deffered length parameter
!*
module m1
	type dtp(l)
		integer, len :: l
		integer :: comp(l)
		real :: com = 1.0
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
		num = i
	end do
	num = i
end do

allocate(point(ubound(matrixA,1),ubound(matrixA,2)), source = matrixA)

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