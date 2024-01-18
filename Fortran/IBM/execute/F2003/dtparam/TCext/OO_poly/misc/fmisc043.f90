! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/misc/fmisc043.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/24/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 311854)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(k1)    ! (4)
        integer, kind :: k1
        procedure(t1), pointer, nopass :: p1 => null()
    end type

    contains

    real function t1()
        pointer t1

        allocate (t1, source=-1.0)
    end function

    subroutine test1 (p1, p2)
        procedure(t1), pointer, intent(inout) :: p1
        real, pointer, intent(in) :: p2

        logical(4) precision_r4

        if (precision_r4(p2, -1.0)) then
            p1 => t1
        end if
    end subroutine
end module

program fmisc043
use m
    type (A(4)) a1

    real, pointer :: r1
    allocate (r1, source=10.2)

    call test1 (a1%p1, r1)

    if (associated(a1%p1)) error stop 1_4

    a1%p1 => t1

    call test1(a1%p1, a1%p1())

    if (.not. associated(a1%p1, t1)) error stop 2_4
end
