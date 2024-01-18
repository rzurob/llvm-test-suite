!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/20/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Diagnostic test case
!                               testing the characteristic of the dummy
!                               arguments for overriding binding.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) data(n)

        contains

        procedure :: select => getDataWithCond
    end type

    abstract interface
        logical function compare4 (b1, b2)
            import
            class(base(4,1)), intent(in) :: b1
            real (4), intent(in) :: b2
        end function

        logical function compare8 (b1, b2)
        import
            class(base(8,1)), intent(in) :: b1
            real(8), intent(in) :: b2
        end function
    end interface

    contains

    class(base(4,:)) function getDataWithCond (b1, proc, b2)
        class(base(4,*)), intent(in) :: b1
        procedure(compare4) proc
        class(base(4,*)), intent(in) :: b2

        pointer getDataWithCond
        nullify(getDataWithCond)
    end function
end module

module m1
use m
    type, extends(base) :: child
        contains

        procedure :: select => getChildValWithCond
    end type

    contains

    class(base(4,:)) function getChildValWithCond (b1, proc, b2)
        class(child(4,*)), intent(in) :: b1
        procedure (compare8) proc  !<-- illegal, however we don't diagnose this
        class(base(8,*)), intent(in) :: b2 !<-- illegal

        pointer getChildValWithCond

        nullify(getChildValWithCond)
    end function
end module

program dtpOverride004d
end
