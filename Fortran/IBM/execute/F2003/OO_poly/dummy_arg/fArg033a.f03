! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (actual-arg changed during
!                               the execution of the procedure)
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
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    type (child) :: c1 = child (1, 'c1')

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (b)
        class (base), target, intent(inout) :: b

        c1%id = c1%id * 10

        call b%print
    end subroutine
end module


program fArg033a
use m
    class (base), allocatable :: b1

    allocate (b1, source=child (1, 'b1'))

    call test1 (c1)

    call test2 (b1)

    call b1%print

    call c1%print

    contains

    subroutine test2 (b)
        class (base), target, intent(out) :: b

        b1%id = 100

        call b%print
    end subroutine
end