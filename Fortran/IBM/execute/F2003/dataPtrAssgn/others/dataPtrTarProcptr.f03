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
!* - data-target is proc pointer which is dummy arg
!* - data-pointer is dummy arg of derived type
!* - lb/ub is dummy arg, ub has optional attribute
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base
	integer :: index
    end type

    interface
	function basefunc(i)
	    import base
	    integer i
	    type(base), pointer :: basefunc(:)
	end function
    end interface


    contains
	subroutine sub(p, pp, lb, ub)
            class(base), pointer :: p(:)
    	    procedure(basefunc), pointer :: pp
	    integer lb,ub
	    optional ub

            if ( .not. present(ub) ) then
	        ! data pointer assignment
	        p(lb:) => pp(lb)
	    else
		p(lb:ub) => pp(ub-lb+1)
	    end if
        end subroutine

end module

    program main
	use m
	class(base), pointer :: pd(:)
	procedure(sub), pointer :: sp
        procedure(basefunc), save, pointer :: pp

	! procedure pointer assignment
	pp => basefunc
	sp => sub

	call sp(pd,pp,9)

	if ( .not. associated(pd) ) error stop 3
	if ( lbound(pd,1) /=9 ) error stop 5
	if ( ubound(pd,1) /= 17 ) error stop 7
	print *, pd%index

        call sp(pd,pp,11,30)

	if ( .not. associated(pd) ) error stop 13
	if ( lbound(pd,1) /=11 ) error stop 15
	if ( ubound(pd,1) /= 30 ) error stop 17
	print *, pd%index


    end program

    function basefunc(i)
 	use m, only : base
	integer i
	type(base), pointer  :: basefunc(:)

	allocate(basefunc(i), source=(/ (base(j),j=1,i) /) )
    end function