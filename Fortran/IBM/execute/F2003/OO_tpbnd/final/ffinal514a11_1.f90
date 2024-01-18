! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of the temp created by
!*                               the function return result; in a print
!*                               statement again)
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

    interface makeData
        function makeBase (i)
        import base
            type(base) :: makeBase
            integer*4, intent(in) :: i
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal514a11_1
use m
    print *, makeData(10)

    print *, 'end'
end

type(base) function makeBase (i)
use m, only: base
    integer*4, intent(in) :: i

    makeBase%id = i
end function
