!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/10/2005
!*
!*  DESCRIPTION                : final sub (private parent type will be
!                               finalized)
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
    type, private :: base
        class (*), pointer :: data => null()
        contains

        final :: finalizeBase
    end type

    type, extends (base) :: child
        character (20) name
    end type

    private finalizeBase

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        if (associated (b%data)) then
            print *, 'deallocating data'

            deallocate (b%data)
        end if
    end subroutine

    elemental subroutine assgnVal (c, data, name)
        class (child), intent(inout) :: c
        class (*), intent(in) :: data
        character(*), intent(in) :: name

        allocate (c%data, source=data)

        c%name = name
    end subroutine
end module

module m1
use m
    type, extends(child) :: gen3
        integer (8) id
    end type
end module

program ffinal011a
use m1
    class (child), pointer :: c1, c2(:)

    allocate (gen3 :: c1, c2(2))

    call assgnVal (c1, 1.5e0_8, 'abc')

    call assgnVal (c2, 'ibm', (/'xyz', 'abc'/))

    print *, 'test 1'
    deallocate (c1)

    print *, 'test 2'

    deallocate (c2)

    print *, 'end'
end
