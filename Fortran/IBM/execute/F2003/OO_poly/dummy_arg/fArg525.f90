!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 03/15/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dummy argument (external procedure as the
!                               actual-arg; used as the defined operator and
!                               assignment)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4) id
    end type

    interface
        type(base) function add (b1, i1)
        import base
            class (base), intent(in) :: b1
            integer(4), intent(in) :: i1
        end function

        subroutine assgnBase (b1, b2)
        import base
            class (base), intent(out) :: b1
            class (base), intent(in) :: b2
        end subroutine
    end interface
end module

program fArg525
use m
    type (base) b1

    b1 = base(20)

    call test1 (b1, add, assgnBase)

    if (b1%id /= 30) error stop 1_4

    contains

    subroutine test1 (b1, proc, assgnProc)
        class (base), intent(inout) :: b1

        interface operator (+)
            type(base) function proc (b1, i1)
            use m, only:base
                class (base), intent(in) :: b1
                integer(4), intent(in) :: i1
            end function
        end interface

        interface assignment (=)
            subroutine assgnProc (b1, b2)
            use m, only: base
                class(base), intent(out) :: b1
                class (base), intent(in) :: b2
            end subroutine
        end interface

        b1 = b1 + 10
    end subroutine
end


type (base) function add (b1, i1)
use m, only : base
    class (base), intent(in) :: b1
    integer(4), intent(in) :: i1

    add%id = b1%id + i1
end function


subroutine assgnBase (b1, b2)
use m, only : base
    class (base), intent(out) :: b1
    class (base), intent(in) :: b2

    b1%id = b2%id
end subroutine
