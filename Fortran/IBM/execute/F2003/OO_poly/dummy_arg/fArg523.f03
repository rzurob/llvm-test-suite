! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (array constructor
!                               containing poly-enetities used as dummy-arg)
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
        integer(4) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent (in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBaseArray (b)
        class (base), intent(in) :: b(:)

        do i = 1, size(b)
            call b(i)%print
        end do
    end subroutine
end module

program fArg523
use m
    class (base), allocatable :: b1
    class (base), pointer :: b2(:)

    type (child), target :: c1(10)

    c1%id = (/(i, i=1, 10)/)
    c1%name = 'xlftest team'

    b2 => c1(::2)

    allocate (b1, source=child(100, 'b2 scalar'))

    call printBaseArray ((/b1, b2/))
end
