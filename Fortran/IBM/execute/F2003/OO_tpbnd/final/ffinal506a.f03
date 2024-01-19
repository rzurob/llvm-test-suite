! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/10/2005
!*
!*  DESCRIPTION                : final sub (finalization not occurrs if the
!                               execution terminated due to error condition)
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
        integer, pointer :: id => null()

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(20), pointer :: name => null()

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        if (associated (b%id)) then
            print *, 'deallocating id'

            deallocate (b%id)
        end if
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        if (associated (c%name)) then
            print *, 'deallocating name'

            deallocate (c%name)
        end if
    end subroutine
end module

program ffinal506a
use m
    class (base), allocatable :: b1
    character(20), target :: c1

    allocate (child :: b1)

    allocate (b1%id, source= 100)

    select type (b1)
        type is (child)
            b1%name => c1
        class default
            error stop 1_4
    end select

    deallocate (b1)     !<-- this will fail and terminate the execution

    print *, 'end'      !<-- this line never prints out
end
