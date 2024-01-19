! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/24/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Use of the parent component in the
!                               structure constructor.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module pointDef
    type point (k)
        integer, kind :: k = 8

        real(k) :: x, y
    end type

    type, extends(point) :: point3D
        real(k) :: z
    end type

    type, extends(point3D) :: colorPoint3D (ck)
        integer, kind :: ck = 1

        integer(ck) :: color
    end type
end module

program dtparamConstr011
use pointDef
    type(point3D(4)) :: p3
    type (colorPoint3D(8, 2)) :: cp1

    logical(4), external :: precision_r4, precision_r8

    p3 = point3D(4)(point=point(4)(2.1e0, 1.2e0), z = 3.3e0)

    cp1 = colorPoint3D(8, 2)(point3D=point3D(8)(point=point(1.1d0, 2.1d0), &
            z = 3.2d0), color = 10)

    !! verify
    if (.not. precision_r4(p3%x, 2.1e0)) error stop 1_4
    if (.not. precision_r4(p3%y, 1.2e0)) error stop 2_4
    if (.not. precision_r4(p3%z, 3.3e0)) error stop 3_4

    if (.not. precision_r8(cp1%x, 1.1d0)) error stop 4_4
    if (.not. precision_r8(cp1%y, 2.1d0)) error stop 5_4
    if (.not. precision_r8(cp1%z, 3.2d0)) error stop 6_4

    if (cp1%color /= 10) error stop 7_4
end
