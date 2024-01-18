!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/15/2005
!*
!*  DESCRIPTION                : final sub (finalization of function results
!                               used in specification expression)
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
        integer*4 :: id = 1

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    interface
        pure type (base) function func1 (i)
        import base
            integer*4, intent(in) :: i
            dimension func1 (10:i+10)
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine abc (func, i1)
        interface
            pure type (base) function func (i)
            import base
                integer*4, intent(in) :: i
                dimension func (10:i+10)
            end function
        end interface

        integer(4), intent(in) :: i1 (lbound(func(5), 1):ubound(func(10), 1))

        print *, 'start abc'

        print *, lbound(func(4)), ubound(func(10))
        print *, size(i1)
    end subroutine
end module

program ffinal516
use m
    procedure(func1) produceArray

    integer(4) i11(11)

    call abc (produceArray, i11)
end

pure type (base) function produceArray (i)
use m, only: base
    integer*4, intent(in) :: i
    dimension produceArray (10:i+10)

    produceArray%id = 0
end function

