! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (for intent(out), non-poly dummy-arg
!*                               associated with poly-actual-arg with different
!*                               dynamic type, then the finalization and default
!*                               initialization only applies to the part that is
!*                               associated with the dummy-arg)
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
        integer*4 :: id

        contains

        final :: finalizeBase
        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = ''

        contains

        procedure :: print => printChild
        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild(b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program ffinal509
use m
    type (child), target :: c1
    class (base), pointer :: b_ptr

    c1%name = 'c1'

    b_ptr => c1

    call sub (b_ptr)

    call b_ptr%print

    contains

    subroutine sub (b)
        type (base), intent(out) :: b

        b%id = 10
    end subroutine
end