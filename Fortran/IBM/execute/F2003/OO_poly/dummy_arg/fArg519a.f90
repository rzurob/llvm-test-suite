! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited poly-dummy-arg
!                               with INTENT(OUT) will cause finalizations of the
!                               actual arg)
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

program fArg519a
use m
    type (base) :: b1

    allocate (b1%value, source = 10)

    call abc (b1)

    if (associated (b1%value)) error stop 1_4
end
