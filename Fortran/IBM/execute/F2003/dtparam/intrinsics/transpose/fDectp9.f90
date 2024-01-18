!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Adrian Green
!*  DATE                       : July 27, 2008
!*  ORIGIN                     : XLF Compiler Test,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Transpose Intrinsic function with derived type parameters.
!*  DESCRIPTION                : Description: uses external functions for matrix multiplication, 
!*                               with an interfaced operator.
!*
!*
!*
!*
type dtp (k)
	integer, kind :: k
	character(k) :: c
end type

type (dtp(1)), parameter :: dtp1(9) = (/dtp(1)('A'), dtp(1)('B'), dtp(1)('C'), &
dtp(1)('D'), dtp(1)('E'), dtp(1)('F'), &
dtp(1)('G'), dtp(1)('H'), dtp(1)('I')/)
type (dtp(1)), parameter :: dtp1a(9) = (/dtp(1)('A'), dtp(1)('D'), dtp(1)('G'), &
dtp(1)('B'), dtp(1)('E'), dtp(1)('H'), &
dtp(1)('C'), dtp(1)('F'), dtp(1)('I')/)
type (dtp(1)), parameter :: dtp2a(3,3) = reshape(dtp1a,(/3,3/))
type (dtp(1)), parameter :: dtp2(3,3) = reshape(dtp1, (/3, 3/))
type (dtp(1)):: res(3,3) = transpose(dtp2)   
do i = 1,3
	do j =1,3
		if( res(i,j)%c /= dtp2a(i,j)%c) STOP 1
	end do
end do
end