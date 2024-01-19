! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of the temps created by
!*                               function result in ASSOCIATE construct)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id = 1

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
        b%id = -1
    end subroutine
end module

program ffinal514a14
use m
    interface
        type(base) function makeData (i)
        use m
            integer*4, intent(in) :: i
        end function
    end interface

    associate (x => makeData(10))
        print *, x
    end associate

    print *, 'end'
end

type(base) function makeData (i)
use m
    integer*4, intent(in) :: i

    makeData%id = i
end function
