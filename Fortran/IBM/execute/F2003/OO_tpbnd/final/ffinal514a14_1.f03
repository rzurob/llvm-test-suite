! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of temps created in
!                               ASSOCIATE construct)
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
    end subroutine
end module

program ffinal514a14_1
use m
    interface
        type(base) function makeData (i)
        use m
            integer*4, intent(in) :: i
        end function
    end interface

    associate (x => makeData(10))
        print *, x, x%id
    end associate

    print *, 'end'
end

type(base) function makeData (i)
use m
    integer*4, intent(in) :: i

    makeData%id = i
end function
