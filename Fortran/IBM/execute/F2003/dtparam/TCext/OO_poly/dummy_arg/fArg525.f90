! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg525.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/15/2005
!*
!*  DESCRIPTION                : dummy argument (external procedure as the
!                               actual-arg; used as the defined operator and
!                               assignment)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    interface
        type(base(4)) function add (b1, i1)
        import base
            class (base(4)), intent(in) :: b1
            integer(4), intent(in) :: i1
        end function

        subroutine assgnBase (b1, b2)
        import base
            class (base(4)), intent(out) :: b1
            class (base(4)), intent(in) :: b2
        end subroutine
    end interface
end module

program fArg525
use m
    type (base(4)) b1

    b1 = base(4)(20)

    call test1 (b1, add, assgnBase)

    if (b1%id /= 30) error stop 1_4

    contains

    subroutine test1 (b1, proc, assgnProc)
        class (base(4)), intent(inout) :: b1

        interface operator (+)
            type(base(4)) function proc (b1, i1)
            use m, only:base
                class (base(4)), intent(in) :: b1
                integer(4), intent(in) :: i1
            end function
        end interface

        interface assignment (=)
            subroutine assgnProc (b1, b2)
            use m, only: base
                class(base(4)), intent(out) :: b1
                class (base(4)), intent(in) :: b2
            end subroutine
        end interface

        b1 = b1 + 10
    end subroutine
end


type (base(4)) function add (b1, i1)
use m, only : base
    class (base(4)), intent(in) :: b1
    integer(4), intent(in) :: i1

    add%id = b1%id + i1
end function


subroutine assgnBase (b1, b2)
use m, only : base
    class (base(4)), intent(out) :: b1
    class (base(4)), intent(in) :: b2

    b1%id = b2%id
end subroutine
