! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/09/2005
!*
!*  DESCRIPTION                : final sub (parent components finalization in
!                               step 3; will start the finalization chain)
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
    type A
        integer(4), pointer :: id(:)

        contains

        final :: finalizeA
    end type

    type base

    end type

    type, extends (base) :: child
        type (A) a1
        character(20), pointer :: name(:)

        contains

        final :: finalizeChild
    end type

    type, extends (child) :: gen3
    end type

    contains

    subroutine finalizeA (a1)
        type (A), intent(inout) :: a1

        if (associated (a1%id)) then
            print *, 'deallocating id'

            deallocate (a1%id)
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

program ffinal008
use m
    class (base), pointer :: b1, b2(:)

    allocate (gen3:: b1, b2(0:1))

    select type (b1)
        class is (child)
            allocate (b1%a1%id(2), source= (/1, 2/))
            allocate (b1%name(2), source=(/'abc', 'xyz'/))
        class default
            error stop 1_4
    end select

    print *, 'test 1'
    deallocate (b1)

    select type (b2)
        type is (gen3)
            allocate (b2(0)%a1%id(2), source= (/10, 20/))
            allocate (b2(1)%a1%id(2), source=(/-1, -2/))
            allocate (b2(0)%name(0:1), source= (/'xlf', 'com'/))
            allocate (b2(1)%name(0:1), source= (/'xlf', 'com'/))
        class default
            error stop 2_4
    end select

    print *, 'test 2'

    deallocate (b2)

    print *, 'end'
end
