! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) for unlimited
!                               poly actual-arg)
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
        integer*4, pointer :: value => null()

        contains

        final :: finalizeBase
    end type

    type container
        class (base), allocatable :: data
    end type

    contains

    subroutine abc (x)
        class (*), intent(out) :: x
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%value)) then
            deallocate (b%value)
            print *, 'data deallocated'
        end if
    end subroutine
end module

program fArg519a1
use m
    class (*), pointer :: x

    allocate (base:: x)

    call abc (x)

    print *, '2nd test'

    allocate (container :: x)

    select type (x)
        class is (container)
            allocate (x%data)
        class default
            error stop 1_4
    end select

    call abc(x)

    print *, 'end'
end
