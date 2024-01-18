!*  ===================================================================
!*
!*  DATE                       : July 27, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!       Unpack Intrinsic function with derived type parameters.
!*  DESCRIPTION                : UNPACK used in initialization expression with derived type parameter
!*

module m1
type dtp (k)
	integer, kind :: k
	integer(k) :: c
end type
end module m1

program a
use m1
type (dtp(1)), parameter :: dtp3(3) = (/dtp(1)(1), dtp(1)(2), dtp(1)(3)/)
type (dtp(1)), parameter :: dtp1(9) = (/dtp(1)(10), dtp(1)(11), dtp(1)(12), &
dtp(1)(13), dtp(1)(14), dtp(1)(15), &
dtp(1)(16), dtp(1)(17), dtp(1)(18)/)
type (dtp(1)), parameter :: dtp2(3,3)= reshape(dtp1, (/3, 3/))
type (dtp(1)):: res(3,3) = unpack(dtp3, reshape((/.TRUE., .FALSE., .FALSE., .TRUE., &
.FALSE., .FALSE., .TRUE., .FALSE., .FALSE./), (/3,3/)), dtp2)

print *, res(1,1)%c, '  ', res(1,2)%c, '  ', res(1,3)%c
print *, res(2,1)%c, ' ', res(2,2)%c, ' ', res(2,3)%c
print *, res(3,1)%c, ' ', res(3,2)%c, ' ', res(3,3)%c

end program a