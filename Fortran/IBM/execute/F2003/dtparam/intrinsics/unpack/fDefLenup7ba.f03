!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Unpack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Uses unpack with a derived type component with a deffered
!*								length parameter. Derived type has real and integer array
!*                              components. Derived type component has allocatable attribute.
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
type (dtp(:)), allocatable :: point(:)
type (dtp(:)), allocatable :: matrixA(:) !dimension(8)
type (dtp(:)), allocatable :: field(:,:), res(:,:) !dimension(4,4)
logical :: mask1(4,4)
integer :: num
num = 1

allocate(dtp(3) :: matrixA(8))
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
	matrixA(i)%com=20.0
	num = i
end do
allocate(point(lbound(matrixA,1):ubound(matrixA,1)), source = matrixA)
allocate(dtp(3)::field(4,4))
do i = 1,4
	do k=1,4
		do j = 1,3
			field(k,i)%comp(j) = 1
		end do
		field(k,i)%com = 1.0
	end do
end do

allocate( res(lbound(unpack (point, mask1, field),1):ubound(unpack (point, mask1, field),1), &
	lbound(unpack (point, mask1, field),2):ubound(unpack (point, mask1, field),2)) &
	, SOURCE = unpack (point, mask1, field))

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