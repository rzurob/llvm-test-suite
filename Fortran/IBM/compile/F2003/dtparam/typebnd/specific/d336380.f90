!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/02/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 336380)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base! (n)
        real(8) :: data

        contains

        procedure, nopass :: p => sub1
        procedure, nopass :: q => sub11
        procedure :: r => sub12
    end type

    contains

    subroutine sub1 (b)
        class(base), intent(inout) :: b
    end subroutine

    subroutine sub11 (b1, b2)
        real b1, b2
    end subroutine

    subroutine sub12 (b, c)
        class(base) b
        type(base) c
    end subroutine
end module

module m1
use m
    type, extends(base) :: child! (m)
        character :: name

        contains

        procedure, nopass :: p => sub2  !<-- illegal
        procedure, nopass :: q => sub21 !<-- illegal
        procedure :: r => sub22 !<-- illegal
    end type

    contains

    subroutine sub2 (b)
        class(child), intent(inout) :: b
    end subroutine

    subroutine sub21 (b1, b2)
        integer b1, b2
    end subroutine

    subroutine sub22 (b, c)
        class(child) b
        integer c
    end subroutine
end module

end
