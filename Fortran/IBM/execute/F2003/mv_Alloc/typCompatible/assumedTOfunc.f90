! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*			         TO of type character(*), ext function name
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

        character(8), allocatable :: a

        interface
            character(8) function func(a)
                character(8), allocatable :: a
                allocatable :: func
            end function
        end interface

        allocate(a, source='lowhight')

        if ( func(a) /= 'lowhight' ) error stop 21
        if ( allocated(a) ) error stop 25

end
        character(8) function func(a)
             character(8), allocatable :: a
             allocatable :: func
             call move_alloc(a, func)

             if ( .not. allocated(func) ) error stop 23

        end function

