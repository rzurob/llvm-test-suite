! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrSpreadReal.f
! opt variations: -qnol -qnodeferredlp

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

    type base(n1,k1)    ! (20,4)
	integer, kind     :: k1
	integer, len      :: n1
	real(k1), pointer :: x(:)
 	procedure(basesub), nopass, pointer :: pp
    end type

    real, target, allocatable :: arr(:)

    contains

	subroutine basesub(i, lbound, b)

	    integer i, lbound
	    optional :: lbound
            type(base(:,4)), pointer :: tmp(:)
	    type(base(*,4)) :: b

	    allocate(tmp(i), source=(/ (base(20,4)( arr,null() ),j=1,i) /) )

	    b%x(lbound:) => tmp(1)%x(::2)

	end subroutine

end module

    program main
	use m

	type(base(20,4)) :: b1
	procedure(basesub), pointer :: pp
	type(base(20,4)) :: b2

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
