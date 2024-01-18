! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (explicit interface is not
!                               required for some situations: dummy-arg is
!                               non-poly scalar)
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

program fArg026
use m
    type (base) :: b1
    class (base), allocatable :: b2
    type (child), target :: c1 = child (100, 'c1')

    class (base), pointer :: b3

    allocate (b2, source = child(1, 'b2'))

    b1 = base (10)

    b3 => c1

    call abc (b1)

    call abc (b2)

    call abc (b3)
end

subroutine abc (b)
    use m
    type (base), intent(in) :: b

    call b%print
end subroutine
