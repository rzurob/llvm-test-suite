!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Transpose Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Description: uses external functions for matrix multiplication,
!*                               with an interfaced operator.
!*
type dtp (k)
	integer, kind :: k
	integer(k) :: c
end type

type (dtp(1)), parameter :: dtp1(9) = (/dtp(1)(1), dtp(1)(2), dtp(1)(3), &
dtp(1)(4), dtp(1)(5), dtp(1)(6), &
dtp(1)(7), dtp(1)(8), dtp(1)(9)/)
type (dtp(1)), parameter :: dtp1a(9) = (/dtp(1)(1), dtp(1)(4), dtp(1)(7), &
dtp(1)(2), dtp(1)(5), dtp(1)(8), &
dtp(1)(3), dtp(1)(6), dtp(1)(9)/)
type (dtp(1)), parameter :: dtp2a(3,3) = reshape(dtp1a,(/3,3/))
type (dtp(1)), parameter :: dtp2(3,3) = reshape(dtp1, (/3, 3/))
type (dtp(1)):: res(3,3) = transpose(dtp2)
do i = 1,3
	do j =1,3
		if( res(i,j)%c /= dtp2a(i,j)%c) STOP 1
	end do
end do
end