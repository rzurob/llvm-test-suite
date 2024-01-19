! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/09/2005
!*
!*  DESCRIPTION                : final sub (finalization of the parent
!                               components)
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
        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(15) :: name
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal007
use m
    type (child), pointer :: c1

    allocate (c1)

    deallocate (c1)

    print *, 'end'
end
