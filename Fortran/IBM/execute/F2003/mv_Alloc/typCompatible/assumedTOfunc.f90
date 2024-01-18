! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : assumedTOfunc.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
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

        if ( func(a) /= 'lowhight' ) stop 21
        if ( allocated(a) ) stop 25

end
        character(8) function func(a)
             character(8), allocatable :: a
             allocatable :: func
             call move_alloc(a, func)

             if ( .not. allocated(func) ) stop 23
 
        end function 
 
