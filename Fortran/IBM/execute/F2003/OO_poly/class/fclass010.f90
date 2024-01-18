! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (user defined operator (**), and
!                               for scalars)
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
        real(8) :: data = -1.d0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        integer(8) :: id = 1

        contains

        procedure :: print => printChild
    end type

    interface operator(**)
        class (base) function power (b1, p)
        import base
            class (base), intent(in) :: b1
            integer, intent(in) :: p
            allocatable power
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        write (*, '(f10.2)') b%data
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        write (*, '(f10.2,i9)') b%data, b%id
    end subroutine
end module


class (base) function power (b1, p)
use m, only:base, child
    class (base), intent(in) :: b1
    integer, intent(in) :: p
    allocatable power

    if (p < 0) allocate (power, source=b1)

    select type (b1)
        type is (base)
            allocate (power, source=base(b1%data**p))
        type is (child)
            allocate (power, source=child(b1%data**p, b1%id**p))
        class default
            error stop 20_4
    end select
end function


program fclass010
use m
    class (base), allocatable :: b1
    class (base), pointer :: b2
    type (child) c1

    c1 = child (2.5001d0, 10)   !<-- use 2.5001, not 2.5

    allocate (b1, source=child(1.5d0, 2_8))

    associate (x => b1**2)
        call x%print
    end associate

    associate (x => c1**3)
        call x%print
    end associate
end

