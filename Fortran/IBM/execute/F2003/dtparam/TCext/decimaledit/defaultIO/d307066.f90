! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/decimaledit/defaultIO/d307066.f
! opt variations: -ql

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
    type base(k1)    ! (4)
        integer, kind :: k1
        real(k1)         id
    end type

    interface abs
        type (base(4)) function babs (b1)
        import base
            type (base(4)), intent(in) :: b1
        end function
    end interface
end module

type (base(4)) function babs (b1)
use m, only: base
    type(base(4)), intent(in) :: b1

    babs%id = abs(b1%id) + 1.0
end function

use m
    intrinsic abs

    real(4) :: r11 = -1.11

    type(base(4)) b11

    logical(4), external :: precision_r4

    b11 = base(4)(-100)

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
        procedure(type(base(4))) func
        type(base(4)) :: b1

        b1 = func(b1)

        write (*, '(e12.4)', decimal='Comma') b1
    end subroutine
end

