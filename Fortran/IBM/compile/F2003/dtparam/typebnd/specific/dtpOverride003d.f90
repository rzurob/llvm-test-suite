!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/11/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               Diagnostic: specific type bound (The
!                               characteristic of the dummy argument of the
!                               overriding binding: assumed --> init-expr.;
!                               init-expr --> assumed)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        contains

        procedure :: p => procP
        procedure, nopass :: q => procQ
    end type

    type, extends(base) :: child1
        character (n) :: name

        contains

        procedure :: p => procChildP
    end type

    type, extends(base) :: child2
        integer id(n)

        contains

        procedure, nopass :: q => procChildQ
    end type

    contains

    function procP (b1, b2)
        class(base(*)), intent(in) :: b1, b2

        procP = 1.0
    end function

    subroutine procQ (b)
        type(base(10)) b
    end subroutine

    function procChildP (b1, b2)
        class(child1(*)), intent(in) :: b1
        class(base(20)), intent(in) :: b2       !<-- illegal to use 20 for n

        procChildP = 2.0
    end function

    subroutine procChildQ (b)
        type(base(*)) b                         !<-- illegal to use assumed
    end subroutine
end module

end
