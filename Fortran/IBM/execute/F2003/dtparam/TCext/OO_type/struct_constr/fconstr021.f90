! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr021.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr021.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (struct_constr in
!*                               struct_constr; inheritance relation)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(k1)    ! (4)
        integer, kind :: k1
        real(k1)      :: x, y
    end type
end module

module m1
use m
    type, extends(point) :: point3D(k2)    ! (4,4)
        integer, kind :: k2
        real(k2)      :: z
    end type

    type (point3D(4,4)) :: p3d1_m = point3D(4,4) (point = point(4) (x = 0.0, y = 0.0), &
                                    z = 0.0)

    type (point3D(4,4)) :: p3d2_m = point3D(4,4) (z = 1.0, point = point(4) (1.0, 1.0))
end module

program fconstr021
use m1

    type, extends(point3D) :: colorPoint3D(k3)    ! (4,4,1)
        integer, kind :: k3
        integer(k3)   :: color = 0   ! it should be enumerated
    end type

    type (point(4)) :: p2_1 = point(4) (0.0, 0.0)
    type (point(4)) :: p2_2 = point(4) (1.0, 1.0)

    type (point3D(4,4)) :: p3_1 = point3D(4,4) (point = point(4)(0.0, 0.0), z = 0.0)
    type (point3D(4,4)) :: p3_2

    type (colorPoint3D(4,4,1)) :: p3c_1 = colorPoint3D(4,4,1) &
            (point = point(4)(0.0, 0.0), z = 0.0)

    type (colorPoint3D(4,4,1)) :: p3c_2 = colorPoint3D(4,4,1) &
            (point3D = point3D(4,4) (point = point(4)(1.0, 1.0), z = 1.0), color = 1)

    type (colorPoint3D(4,4,1)) :: p3c_3 = colorPoint3D(4,4,1) &
            (color = 2, point3D = point3D(4,4) (1.0, 1.0, 1.0))

    type (colorPoint3D(4,4,1)) :: p3c_4

    p3_2 = point3D(4,4) (point = point(4) (1.0, 1.0), z = 1.0)

    p3c_4 = colorPoint3D(4,4,1) (color = 1, z = 0.0, point = p2_1)

    ! validate variables
    if (.not. valid3DPoint (p3_1, 0.0, 0.0, 0.0)) error stop 1_4

    if (.not. valid3DPoint (p3_2, 1.0, 1.0, 1.0)) error stop 2_4

    if (.not. valid3DPoint (p3c_1%point3D, 0.0, 0.0, 0.0)) error stop 3_4
    if (p3c_1%color /= 0) error stop 4_4

    if (.not. valid3DPoint (p3c_2%point3D, 1.0, 1.0, 1.0)) error stop 5_4
    if (p3c_2%color /= 1) error stop 6_4

    if (.not. valid3DPoint (p3c_3%point3D, 1.0, 1.0, 1.0)) error stop 7_4
    if (p3c_3%color /= 2) error stop 8_4

    if (.not. valid3DPoint (p3c_4%point3D, 0.0, 0.0, 0.0)) error stop 9_4
    if (p3c_4%color /=1) error stop 10_4


    ! reset some of the variables
    p3_2 = point3D(4,4) (z = 0.0, point = p2_2)

    if (.not. valid3DPoint (p3_2, 1.0, 1.0, 0.0)) error stop 11_4

    p3c_4 = colorPoint3D(4,4,1) (point3D = p3_2)

    if (.not. valid3DPoint (p3c_4%point3D, p3_2%x, p3_2%y, p3_2%z)) &
                    error stop 12_4

    if (p3c_4%color /= 0) error stop 13_4


    p3c_4 = colorPoint3D(4,4,1) (point3D = point3D(4,4) (z = 1.0, point = p2_1), color = 2)

    if (.not. valid3DPoint (p3c_4%point3D, 0.0, 0.0, 1.0)) error stop 14_4
    if (p3c_4%color /= 2) error stop 15_4


    !! validate module variables
    if (.not. valid3DPoint (p3d1_m, 0.0, 0.0, 0.0)) error stop 16_4

    if (.not. valid3DPoint (p3d2_m, 1.0, 1.0, 1.0)) error stop 17_4

    contains

    logical function valid3DPoint (p3, x, y, z)
        type (point3D(4,4)), intent(in) :: p3
        real*4, intent(in) :: x, y, z

        valid3DPoint = ((p3%x == x) .and. (p3%y == y) .and. (p3%z == z))
    end function
end
