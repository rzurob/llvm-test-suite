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
!* - ub of data-pointer is intrinsic func
!* - data-pointer/target have attributes save, public, volatile
!* -
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base
	integer :: p(10)
    end type

    type another
        class(*), pointer :: p(:)
    end type

    type(another), public, save, volatile, target :: a

end module

program main
    use m

    type(base), volatile, target :: b

    data ( b%p(i), i = 2,6) / 1,2,3,4,5/

    a%p(ishft(1, 2):int(7.0)) => b%p(b%p(3) : b%p(6))

    if ( .not. associated(a%p) ) error stop 9
    if ( lbound( a%p, 1) /= 4 ) error stop 19
    if ( ubound( a%p, 1) /= 7 ) error stop 29

    select type ( y => a%p)
	type is (integer)
	    if ( any(y .ne. (/ 1, 2, 3, 4 /)) ) error stop 39
	class default
	    stop 22
	end select

end