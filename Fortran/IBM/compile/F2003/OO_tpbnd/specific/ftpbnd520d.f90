! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/11/2005
!*
!*  DESCRIPTION                : specific type bound (diagnostic testing on
!                               protected attributes)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id = 1

        contains

        procedure :: print => printBase
        procedure :: addID => addID2Base
    end type

    type (base), protected, target :: b1_m = base (10)

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine addID2Base (b, i)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = b%id + i
    end subroutine
end module

program ftpbnd520d
use m
    class (*), pointer :: x1

    associate (x => b1_m)
        call x%addID (9)    !<-- illegal

        x1 => x             !<-- illegal

        x%id = 100          !<-- illegal
    end associate

    call b1_m%print
end
