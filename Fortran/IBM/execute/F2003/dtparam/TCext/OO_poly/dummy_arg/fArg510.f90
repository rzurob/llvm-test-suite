! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg510.f
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
! %GROUP: fArg510.f
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
!*  DATE                       : 04/02/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : dummy-arg (poly-dummy-arg in the operator
!*                               interface declaration; also tests the floating
!*                               point algorithm to ensure the precison)
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

    type, extends(point) :: point3D    ! (4)
        real(k1) :: z
    end type

    contains

    pure logical function floatEqual (r1, r2)
        real*4, intent(in) :: r1, r2

        real*4, parameter :: e = 0.0000005  ! 5.0e-7

        floatEqual = (abs (r1 - r2) <= abs (r1+r2)*e)
    end function
end module

module m1
use m
    !! every interface from now on has to be checked for ambiguity
    interface operator (==)
        elemental logical function pointEqual (p1, p2)
        use m
            class (point(4)), intent(in) :: p1
            type (point(4)), intent(in) :: p2
        end function

        elemental logical function point3DEqual (p1, p2)
        use m
            class (point3D(4)), intent(in) :: p1
            type (point3D(4)), intent(in) :: p2
        end function
    end interface
end module

program fArg510
use m1
    type (point(4)) :: p1, p2

    type (point3D(4)) :: p3d_1, p3d_2

    p1 = point(4) (1.543, 1.345)

    p2%x = (p1%x * 0.0000423) / 0.0000423
    p2%y = p1%y + 1.345e-8

    if (.not. (p1 == p2)) error stop 1_4

    p3d_1 = point3D(4) (0.24345, -1.24354, 0.68731)

    p3d_2%x = (p3d_1%x * .00486524) / .00486524
    p3d_2%y = (p3d_1%y - 8.243e-8) + 3.35782e-8
    p3d_2%z = (p3d_1%z + 2.2435485) - 2.2435485

    if (.not. (p3d_1 == p3d_2)) error stop 2_4

    p3d_2%z = p3d_1%z + 1.0e-6

    if (p3d_1 == p3d_2) error stop 3_4
end

elemental logical function pointEqual (p1, p2)
use m
    class (point(4)), intent(in) :: p1
    type (point(4)), intent(in) :: p2


    pointEqual = (floatEqual (p1%x, p2%x) .and. &
                  floatEqual (p1%y, p2%y))
end function

elemental logical function point3DEqual (p1, p2)
use m
    class (point3D(4)), intent(in) :: p1
    type (point3D(4)), intent(in) :: p2


     point3DEqual = (floatEqual (p1%x, p2%x) .and. &
                   floatEqual (p1%y, p2%y) .and. &
                   floatEqual (p1%z, p2%z))
end function

