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
!* - data-ptr assgnment appears in associate construct
!* - data-target is the selector of associate construct
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
    complex(4), target, allocatable :: c4(:)
    class(*), pointer :: ptr(:)

    allocate(c4(10), source = (/(cmplx(i-1,i+1,4), i=1,10)/))

    associate( x => c4)
    	ptr(2:) => x(10:1:-1)

        if ( .not. associated(ptr, x(10:1:-1))) error stop 1

        select type(y => ptr)
	    type is (complex(4))
        	if ( lbound(y,1) /= 2 ) error stop 2
        	if ( ubound(y,1) /= 11 ) error stop 3
    	        write (*, '("(",f10.6,", ", f10.6, ")")')  y
    	        write (*, '("(",f10.6,", ", f10.6, ")")')  y/cmplx(2,2,4)
	    class default
	        stop 5
        end select
    end associate

end program