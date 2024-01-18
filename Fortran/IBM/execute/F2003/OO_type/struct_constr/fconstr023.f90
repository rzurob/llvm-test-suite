!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr023.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            :
!*                                                                     
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 12/22/2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (array components
!*                               need to be conformed by shape in 
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
    type point
        real*4 :: x(2)
    end type

    type (point) :: p1_m = point (x = (/0, 0/))
    type (point) :: p2_m = point (1)
end module

module m1
use m
    type, extends(point) :: point3D
        real*4 :: z
    end type

    type (point) :: p3_m = point (x = (/1, 0/))

    type (point3D) :: p3d1_m = point3D (z = 0.0, x = (/1, 2/))
    type (point3D) :: p3d2_m = point3D ((/0.0, 1.0/), z = 1.0)
end module

program fconstr023
use m1

    type, extends(point3D) :: colorPoint3D
        integer*1 :: color = 0   ! it should be enumerated
    end type

    type (point) :: p2_1 = point (1)
    type (point) :: p2_2 = point (x = 10)

    type (point3D) :: p3_1 = point3D (z = 0, point = point(x=2))
    type (point3D) :: p3_2 = point3D (z = 1, x = 1)

    type (colorPoint3D) :: p3c_1 = colorPoint3D (point = point((/5.0, 4.0/)), &
                    color = 1, z = 0.0)

    ! two conversions is done for the following statement
    type (colorPoint3D) :: p3c_2 = colorPoint3D (x = (1.0, 2.0), color = 2, &
                    z = 1.0)

    if ((p2_1%x(1) /= 1.0) .or. (p2_1%x(2) /= 1.0)) error stop 1_4

    if ((p2_2%x(1) /= 10.0) .or. (p2_2%x(2) /= 10.0)) error stop 2_4

    if (.not. valid3DPoint (p3_1, 2.0, 2.0, 0.0)) error stop 3_4

    if (.not. valid3DPoint (p3_2, 1.0, 1.0, 1.0)) error stop 4_4

    if (.not. valid3DPoint (p3c_1%point3D, 5.0, 4.0, 0.0)) error stop 5_4

    if (.not. valid3DPoint (p3c_2%point3D, 1.0, 1.0, 1.0)) error stop 6_4

    ! validate module variables
    if ((p1_m%x(1) /= 0.0) .or. (p1_m%x(2) /= 0.0)) error stop 7_4

    if ((p2_m%x(1) /= 1.0) .or. (p2_m%x(2) /= 1.0)) error stop 8_4

    if ((p3_m%x(1) /= 1.0) .or. (p3_m%x(2) /= 0.0)) error stop 9_4

    if (.not. valid3DPoint (p3d1_m, 1.0, 2.0, 0.0)) error stop 10_4

    if (.not. valid3DPoint (p3d2_m, 0.0, 1.0, 1.0)) error stop 11_4


    contains

    logical function valid3DPoint (p3, x, y, z)
        type (point3D), intent(in) :: p3
        real*4, intent(in) :: x, y, z

        valid3DPoint = ((p3%x(1) == x) .and. (p3%x(2) == y) .and. (p3%z == z))
    end function
end
