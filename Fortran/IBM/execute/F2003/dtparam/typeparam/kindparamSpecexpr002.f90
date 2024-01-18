!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/31/2005
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type parameter used as the size of
!                               the array: general_point given in the standard.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type generalPoint (k, dim)
        integer, kind :: k = kind(0.0)
        integer, kind :: dim

        real(k) coordinates(dim)
    end type
end module

program kindparamSpecexpr002
use m
    type (generalPoint(8, 3)) gp1
    type (generalPoint(dim=2)) gp2

    logical(4), external :: precision_r4, precision_r8

    gp1%coordinates = (/1.31d0, 1.22d0, 3.14d0/)
    gp2%coordinates = 1.31e0

    if (.not. precision_r8(gp1%coordinates(1), 1.31d0)) error stop 1_4
    if (.not. precision_r8(gp1%coordinates(2), 1.22d0)) error stop 2_4
    if (.not. precision_r8(gp1%coordinates(3), 3.14d0)) error stop 3_4

    if (.not. precision_r4(gp2%coordinates(1), 1.31e0)) error stop 4_4
    if (.not. precision_r4(gp2%coordinates(2), 1.31e0)) error stop 5_4

    if ((size(gp1%coordinates) /= 3) .or. (size(gp2%coordinates) /= 2)) &
            error stop 6_4
end
