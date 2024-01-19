! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/01/2005
!*
!*  DESCRIPTION                : final sub (recursive finalization for a
!                               linked-list)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type node
        type (node), pointer :: next => null()

        contains

        final :: finalizeNode
    end type

    contains

    recursive subroutine finalizeNode (b)
        type (node), intent(inout) :: b

        if (associated (b%next)) then
            print *, 'deallocating next node'

            deallocate (b%next)
        end if
    end subroutine
end module

use m
    type (node), allocatable :: b1

    allocate (b1)
    allocate (b1%next)
    allocate (b1%next%next)
    allocate (b1%next%next%next)

    deallocate (b1)

    print *, 'end'
end
