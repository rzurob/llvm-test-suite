! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (TARGET attribute on
!                               passed-object dummy-arg)
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

        procedure :: associate => associateBase
        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    class (base), pointer :: global

    contains

    subroutine associateBase (b, b_ptr)
        class (base), target, intent(in) :: b
        class (base), pointer, intent(out) :: b_ptr

        b_ptr => b
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine
end module

program fArg013a8
use m
    type (base), target :: b1
    type (child), target :: c1

    class (base), pointer :: main1, main2

    b1%id = 1

    c1%id = 2
    c1%name = 'c1'

    main1 => b1

    call main1%associate (global)

    if (.not. associated (global, b1)) error stop 1_4

    call global%print

    call c1%associate (main2)

    if (.not. associated (main2, c1)) error stop 2_4

    call main2%print

    call main2%associate (main1)

    if (.not. associated (main1, c1)) error stop 3_4

    call main1%print
end
