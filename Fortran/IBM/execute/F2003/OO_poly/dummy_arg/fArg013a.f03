! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (test the TARGET attribute
!                               where the pointer associations were tested
!                               before and after the procedure calls)
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

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    class (base), pointer :: b_ptr, b_ptr2

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine assgnb1Tob2 (b1, b2)
        class (base), pointer, intent(out) :: b1
        class (base), target, intent(in) :: b2

        b1 => b2
    end subroutine
end module

program fArg013a
use m
    class (base), pointer :: b1

    type (child), target :: c1

    allocate (b1, source=child (10, 'b1'))

    c1 = child (20, 'c1')

    call assgnb1Tob2 (b_ptr, b1)

    call assgnb1Tob2 (b_ptr2, c1)

    if (.not. associated (b_ptr, b1)) error stop 1_4

    if (.not. associated (b_ptr2, c1)) error stop 2_4

    call b_ptr%print

    call b_ptr2%print

    deallocate (b1)

    call assgnb1Tob2 (b_ptr, b_ptr2)

    if (.not. associated (b_ptr, c1)) error stop 3_4
end
