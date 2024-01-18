!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/30/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 307066)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(4) id
    end type

    interface abs
        type (base) function babs (b1)
        import base
            type (base), intent(in) :: b1
        end function
    end interface
end module

type (base) function babs (b1)
use m, only: base
    type(base), intent(in) :: b1

    babs%id = abs(b1%id) + 1.0
end function

use m
    intrinsic abs

    real(4) :: r11 = -1.11

    type(base) b11

    logical(4), external :: precision_r4

    b11 = base(-100)

    call test1 (abs, r11)

    call test2 (babs, b11)

    if (.not. precision_r4(r11, 1.11_4)) error stop 1_4

    if (.not. precision_r4(b11%id, 1.01e2)) error stop 2_4

    contains

    subroutine test1 (func, r1)
        real(4) func
        real(4) r1

        r1 = func(r1)

        write (*, '(dc, f12.4)') r1
    end subroutine

    subroutine test2 (func, b1)
        procedure(type(base)) func
        type(base) :: b1

        b1 = func(b1)

        write (*, '(e12.4)', decimal='Comma') b1
    end subroutine
end

