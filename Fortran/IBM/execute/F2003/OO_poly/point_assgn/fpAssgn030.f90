! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (pointer assignment
!*                               inside the ASSOCIATE construct)
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

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fpAssgn030
use m
    class (base), pointer :: b_ptr

    type (child), target :: c1 = child (1, 'c1')

    associate (x => c1)
        b_ptr => x

        call b_ptr%print
    end associate

end
