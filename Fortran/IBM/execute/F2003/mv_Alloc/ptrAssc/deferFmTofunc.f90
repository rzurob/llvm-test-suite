! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM & pointer have deferred type parameter
!*			         TO of type character(8), ext function name
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
 	character(:), pointer :: p
end module

use m
        character(:), allocatable :: a

	interface
	    function func(a)
                character(:), target, allocatable :: a
                character(8), target, allocatable :: func
            end function
	end interface

        allocate(a, source='lowhight')

        if ( func(a) /= 'lowhight' ) stop 21

end
    	    character(8) function func(a)
		use m
                character(:), target, allocatable :: a
        	allocatable :: func
                target :: func

 	        if ( allocated(a) ) p => a

                call move_alloc(a, func)

		if ( allocated(func) ) then
		   if ( .not. associated(p, func) ) stop 23
		else
                   stop 25
		endif

            end function
