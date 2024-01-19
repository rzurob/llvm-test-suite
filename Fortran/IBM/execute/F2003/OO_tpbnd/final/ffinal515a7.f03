! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/15/2005
!*
!*  DESCRIPTION                : final sub (finalization of temp created by
!                               structure constructor in where construct; in
!                               mask-expr)
!*
!*  KEYWORD(S)                 :
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

    interface operator (>=)
        elemental logical function b1GEb2 (b1, b2)
        import base
            class (base), intent(in) :: b1, b2
        end function
    end interface


    interface operator (<)
        elemental logical function b1LTb2 (b1, b2)
        import base
            class (base), intent(in) :: b1, b2
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module


elemental logical function b1LTb2 (b1, b2)
use m, only : base
    class (base), intent(in) :: b1, b2

    b1LTb2 = (b1%id < b2%id)
end function

elemental logical function b1GEb2 (b1, b2)
use m, only : base
    class (base), intent(in) :: b1, b2

    b1GEb2 = (b1%id >= b2%id)
end function

program ffinal515a7
use m
    type (base) :: b1 (10)

    b1%id = (/(i,i=1,10)/)

    where (b1 >= base (6))
        b1%id = 5
    elsewhere ((b1 < base(5)) .and. (b1 >= base(2)))
        b1%id = 2
    end where

    print *, b1
end
