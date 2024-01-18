!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*  ORIGIN                     : XLF Compiler Test,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Unpack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : UNPACK uses pack with a derived type component
!*								with a deffered length parameter
!*

module m1
	type dtp(l)
		integer, len :: l
		integer :: comp(l)
		real :: com
	end type dtp
end module m1

program a
use m1
type (dtp(:)), pointer :: point(:)
type (dtp(3)), dimension(8) :: matrixA
type (dtp(3)), dimension(4,4) :: field, res
logical :: mask1(4,4)
integer :: num

do  i=3,4
	do j=1,4
		mask1(1,j)=.TRUE.
		mask1(2,j)=.TRUE.
		mask1(i,j)=.FALSE.
	end do
end do

num = 1
do i = 1,8
	do k = 1,3
		matrixA(i)%comp(k) = num
		num = num + 1
	end do
	num = i
	matrixA(i)%com = 20.0
end do

allocate(point(lbound(matrixA,1):ubound(matrixA,1)), source = matrixA)

do i = 1,4
	do k=1,4
		do j = 1,3
			field(k,i)%comp(j) = 1
		end do
	field(k,i)%com = 1.0
	end do
end do
res = unpack (point, mask1, field)
print *, "comp"
do i = 1,4
	do j=1,4
		print *, res(i,j)%comp(1), '  ', res(i,j)%comp(2), '  ', res(i,j)%comp(3)
	end do
	print *, ' '
end do
print *, "com"
do i = 1,4
	do j=1,4
		print *, res(i,j)%com
	end do
	print *, ' '
end do
end program a