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
!* - data-ptr is used as ac value of array constructor
!* - test opeartor .lt.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

    class(*), pointer :: p(:)

    allocate(p(10), source = (/(i, i=1,10) /))

    p(1:10) => p(10:1:-1)

    if ( .not. associated(p)) error stop 1
    if ( lbound(p,1) /= 1 ) error stop 2
    if ( ubound(p,1) /=10 ) error stop 3

    select type (p)
	type is (integer)
    	    p = p([p])
    	    if ( any ( p .ne. (/ (i,i=1,10)/))) error stop 5
    	    if ( any ( p .lt. (/ (i,i=0,9)/))) error stop 6
	class default
	    stop 7
    end select

end program
