! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (allocatable dummy-arg;
!                               the actual-arg shall be of the same
!                               characteristic)
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
        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        integer*4 :: id
        contains

        procedure :: print => printChild
    end type

    type, extends (child) :: gen3
        character*20 :: name

        contains

        procedure :: print => printGen3
    end type

    contains

    subroutine createBase (b, id, name)
        class (base), allocatable, intent(out) :: b
        integer*4, intent(in) :: id
        character(*), intent(in), optional :: name

        if (present (name)) then
            allocate (b, source=gen3(id = id, name=name))
        else
            allocate (b, source=child(id))
        end if
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printGen3 (b)
        class (gen3), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg005a
use m
    class (base), allocatable :: b1

    call createBase (b1, 1, 'gen3_alloc')

    call b1%print

    call createBase (b1, 10)

    call b1%print
end
