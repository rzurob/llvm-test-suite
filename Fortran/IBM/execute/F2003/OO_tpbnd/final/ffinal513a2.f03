! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (function return result finalized;
!*                               use array)
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
        integer*4 :: id = 0

        contains

        final :: finalizeBase
        final :: finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        implicit type(base) (b)
        dimension b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program ffinal513a2
use m
    interface
        function makeBaseArray (id, n)
        use m
            integer*4, intent(in) :: id, n
            type (base) :: makeBaseArray (n)
        end function
    end interface

    print *, makeBaseArray (10, 2)
end

function makeBaseArray (id, n)
use m
    integer*4, intent(in) :: id, n
    type (base) :: makeBaseArray (n)

    makeBaseArray%id = id
end function