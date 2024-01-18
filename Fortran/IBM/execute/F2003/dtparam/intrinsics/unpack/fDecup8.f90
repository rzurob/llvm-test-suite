!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : UNPACK DTP INTRINSIC FUNCTION
!*
!*  PROGRAMMER                 : Adrian Green
!*  DATE                       : July 27, 2008
!*  ORIGIN                     : XLF Compiler Test,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Unpack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : UNPACK used in initialization expression with derived type parameter
!*							
!*
!*

type dtp (k)
	integer, kind :: k
	character(k) :: c
end type

type (dtp(1)), parameter :: dtp3(3) = (/dtp(1)('X'), dtp(1)('Y'), dtp(1)('Z')/)
type (dtp(1)), parameter :: dtp1(9) = (/dtp(1)('A'), dtp(1)('B'), dtp(1)('C'), &
dtp(1)('D'), dtp(1)('E'), dtp(1)('F'), &
dtp(1)('G'), dtp(1)('H'), dtp(1)('I')/)
type (dtp(1)), parameter :: dtp2(3,3)= reshape(dtp1, (/3, 3/))
type (dtp(1)):: res(3,3) = unpack(dtp3, reshape((/.TRUE., .FALSE., .FALSE., .TRUE., &
.FALSE., .FALSE., .TRUE., .FALSE., .FALSE./), (/3,3/)), dtp2)   

print *, res(1,1)%c, ' ', res(1,2)%c, ' ', res(1,3)%c
print *, res(2,1)%c, ' ', res(2,2)%c, ' ', res(2,3)%c
print *, res(3,1)%c, ' ', res(3,2)%c, ' ', res(3,3)%c
end