!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Pack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : PACK used in initialization expression with derived type parameter
!*
type dtp (k)
	integer, kind :: k
	character(k) :: c
end type

type (dtp(1)), parameter :: dtp1(9) = (/dtp(1)('A'), dtp(1)('B'), dtp(1)('C'), &
dtp(1)('D'), dtp(1)('E'), dtp(1)('F'), &
dtp(1)('G'), dtp(1)('H'), dtp(1)('I')/)
type (dtp(1)), parameter :: dtp2(3,3)= reshape(dtp1, (/3, 3/))
type (dtp(1)):: res(3) = pack(dtp2, reshape((/.TRUE., .FALSE., .FALSE., .TRUE., .FALSE., .FALSE., .TRUE., .FALSE., .FALSE./), (/3,3/)))

print *, res(1)%c, ' ', res(2)%c, ' ', res(3)%c
end