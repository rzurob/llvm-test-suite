! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited-poly allocatable
!                               dummy-arg)
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
        integer(4) :: id

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine deallocateX (x)
        class (*), allocatable, intent(inout) :: x

        if (allocated (x)) then
            deallocate (x)
        else
            print *, 'not allocated'
        end if
    end subroutine

    subroutine resetX (x)
        class (*), allocatable, intent(out) :: x(:)
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child), intent(inout) :: c(:)

        print *, 'finalizeChildRank1 size =', size(c)
    end subroutine

    subroutine finalizeBase (b)
        type (base) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program fArg006a2
use m
    class (*), allocatable :: x1, x2(:)

    print *, 'first call'

    call deallocateX (x1)

    allocate (child :: x1, x2 (2:3))

    print *, 'second call'

    call deallocateX (x1)

    print *, 'last call'

    call resetX (x2)

    if (allocated (x1) .or. allocated (x2)) error stop 1_4
end
