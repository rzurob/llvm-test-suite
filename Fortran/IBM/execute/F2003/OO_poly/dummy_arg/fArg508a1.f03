! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) dummy-arg's
!                               default initialization; poly-actual-arg
!                               associated with non-poly-dummy-arg; use scalars)
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

    type, extends(base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    class (base), allocatable :: b1_m(:)

    contains

    logical function isDefault (b)
        type (base), intent(out) :: b

        isDefault = (b%id == -1)
    end function

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg508a1
use m
    type (child) :: c1 (3)

    c1 = (/child (1,'c1_1'), child(2,'c1_2'), child(3,'c1_3')/)

    allocate (b1_m(3:5), source=c1)

    if (.not. isDefault(b1_m(3))) error stop 1_4

    call b1_m(3)%print
    call b1_m(4)%print
    call b1_m(5)%print
end
