! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (happens in ASSOCIATE
!*                               construct; the associating entity assumes the
!*                               dynamic type of the selector)
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
        integer*4 :: id = -1

        contains

        procedure :: print => printBase
        procedure :: reset => resetBaseVal
    end type

    type, extends(base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
        procedure :: reset => resetChildVal
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

    subroutine resetBaseVal (b)
        class (base), intent(inout) :: b

        b%id = -1
    end subroutine

    subroutine resetChildVal (b)
        class (child), intent(inout) :: b

        b%id = -1
        b%name = 'default'
    end subroutine
end module

program fpAssgn030a
use m
    class (base), pointer :: b_ptr, b_ptr1

    type (child), target :: c1 = child (1, 'c1')

    b_ptr => c1

    associate (x => b_ptr)
        call x%print

        b_ptr1 => x

        call b_ptr1%print

        call x%reset
    end associate

    if (.not. associated (b_ptr, b_ptr1)) error stop 1_4

    if (.not. associated (b_ptr1, c1)) error stop 2_4

    call b_ptr1%print
end
