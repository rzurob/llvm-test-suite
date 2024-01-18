! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specific/ftpbnd526.f
! opt variations: -ql

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/8/2005
!*
!*  DESCRIPTION                : specific type bound (use of the external
!                               procedure for the PASS binding)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base1(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i

        contains
        procedure :: diff
    end type

    type base2(k2)    ! (4)
        integer, kind :: k2
        real(k2)         r

        contains

        procedure, pass(b) :: diff
    end type

    interface
        real function diff (a, b)
        import
            class(base1(4)), intent(in) :: a
            class(base2(4)), intent(in) :: b
        end function
    end interface
end module

program ftpbnd526
use m
    logical(4) precision_r4

    type (base1(4)) a1
    type (base2(4)) b1

    a1 = base1(4) (10)
    b1 = base2(4) (1.0)

    r1 = a1%diff (b1)
    r2 = b1%diff (a1)

    if (.not. precision_r4 (r1, r2)) error stop 1_4
    if (.not. precision_r4 (r1, 10*10.0 - 1.0)) error stop 2_4
end

real function diff (a, b)
use m, only: base1, base2
    class (base1(4)), intent(in) :: a
    class (base2(4)), intent(in) :: b

    diff = a%i * 10.0 - b%r
end function
