! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2005
!*
!*  DESCRIPTION                : data pointer assignment (C721, POINTER
!                               attribute is required for lhs)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn001d2
    class (*), allocatable :: x(:)
    integer*4, target :: i(10)

    x => i      !<-- x not a pointer

    contains

    subroutine abc (x1)
        class (*) x1

        x1 => i1    !<-- x1 not a pointer
    end subroutine

end