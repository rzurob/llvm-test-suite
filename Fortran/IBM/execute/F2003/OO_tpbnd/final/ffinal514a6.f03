! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalizatiion of the temporaries
!*                               created by function calls in if-stmt)
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
        integer*4 :: id

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module


program ffinal514a6
use m
    interface operator (==)
        pure logical function baseEqual (b1, b2)
        use m
            type (base), intent(in) :: b1, b2
        end function
    end interface

    interface
        type (base) function produceBase (i)
        use m
            integer*4, intent(in) :: i
        end function
    end interface

    type(base), save :: b1

    b1%id = 10

    if (b1 == produceBase(10)) print *, 'you should see finalizeBase next line'

    print *, 'end'
end


logical function baseEqual (b1, b2)
use m
    type (base), intent(in) :: b1, b2

    baseEqual = (b1%id == b2%id)
end function


type (base) function produceBase (i)
use m
    integer*4, intent(in) :: i

    produceBase%id = i
end function