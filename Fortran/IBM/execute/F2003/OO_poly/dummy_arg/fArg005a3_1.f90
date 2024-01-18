! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-pointer dummy-arg;
!*                               scalars)
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

    interface
        subroutine createBase (b, id, name)
        import base
            class (base), pointer, intent(out) :: b
            integer*4, intent(in) :: id
            character(*), intent(in), optional :: name
        end subroutine
    end interface

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

program fArg005a3_1
use m
    class (base), pointer :: b1

    type (child), target :: c1

    b1 => c1

    call createBase (b1, 10, name='b1_pointer')

    call b1%print

    deallocate (b1)

    call createBase (b1, 20)

    call b1%print
end


subroutine createBase (b, id, name)
use m, only : base, child
    class (base), pointer, intent(out) :: b
    integer*4, intent(in) :: id
    character(*), intent(in), optional :: name

    if (present (name)) then
        allocate (b, source=child(id, name))
    else
        allocate (b)
        b%id = id
    end if
end subroutine
