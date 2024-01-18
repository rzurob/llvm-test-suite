!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-pointer is component of dummy arg of type bound subroutine
!* - lb is a dummy arg of type bound procedure
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base
	real, pointer :: x(:)
 	procedure(basesub), nopass, pointer :: pp
    end type

    real, target, allocatable :: arr(:)

    contains

	subroutine basesub(i, lbound, b)

	    integer i, lbound
	    optional :: lbound
            type(base), pointer :: tmp(:)
	    type(base) :: b

	    allocate(tmp(i), source=(/ (base( arr,null() ),j=1,i) /) )

	    b%x(lbound:) => tmp(1)%x(::2)

	end subroutine

end module

    program main
	use m

	type(base) :: b1
	procedure(basesub), pointer :: pp
	type(base) :: b2

 	b1%pp => basesub

	allocate(arr(15), source = (/ (real(i), i=-1,-15,-1 )/) )

	call b1%pp(1,10, b2)

        if ( .not. associated(b2%x) ) stop 13
        if ( lbound(b2%x,1) /= 10 ) stop 15
        if ( ubound(b2%x,1) /= 17 ) stop 17

	write(*, '(4f14.8)') b2%x
	print *, shape(spread(b2%x, 2, 2))
	write(*, '(8f14.6)') spread(b2%x,2, 2)

    end program
