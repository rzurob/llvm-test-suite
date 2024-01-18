!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal513a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (intent(out) for polymorphic
!*                               dummy-arg)
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
use ieee_arithmetic
    type point
        real*4 :: x, y

        contains

        procedure :: goodPart => numOfGoodCompPoint
        final :: invalidatePoint
    end type

    contains

    subroutine invalidatePoint (p)
        type (point), intent(inout) :: p

        !!!! NOTE this requires compiler support for IEEE standard
        if (ieee_support_sqrt(-1.0)) then
            p%x = sqrt (-1.)
            p%y = sqrt (-1.0)
        else
            p%x = -1.0
            p%y = -1.0
        end if
    end subroutine

    integer*4 function numOfGoodCompPoint (p)
        class (point), intent(in) :: p

        logical precision_r4

        numOfGoodCompPoint = 0

        if (ieee_support_sqrt(-1.0)) then
            if (.not. ieee_is_nan (p%x)) numOfGoodCompPoint = numOfGoodCompPoint + 1
            if (.not. ieee_is_nan (p%y)) numOfGoodCompPoint = numOfGoodCompPoint + 1
        else
            if (.not. precision_r4 (p%x, -1.)) numOfGoodCompPoint = &
                                numOfGoodCompPoint + 1

            if (.not. precision_r4 (p%y, -1.)) numOfGoodCompPoint = &
                                numOfGoodCompPoint + 1
        end if

    end function
end module

module m1
use ieee_arithmetic
use m, only: point
    type, extends (point) :: point3D
        real*4 :: z

        contains

        procedure :: goodPart => numOfGoodCompPoint3D
        final :: invalidatePoint3D
    end type

    contains

    integer*4 function numOfGoodCompPoint3D (p)
        class (point3D), intent (in) :: p

        logical precision_r4

        numOfGoodCompPoint3D = p%point%goodPart ()

        if (ieee_support_sqrt(-1.0)) then
            if (.not. ieee_is_nan (p%z)) numOfGoodCompPoint3D = &
                            numOfGoodCompPoint3D + 1
        else
            if (.not. precision_r4(p%z, -1.)) numOfGoodCompPoint3D = &
                            numOfGoodCompPoint3D + 1
        end if
    end function

    subroutine invalidatePoint3D (p3)
        type (point3D), intent(inout) :: p3

        if (ieee_support_sqrt(-1.0)) then
            p3%z = sqrt(-1.0)
        else
            p3%z = -1.0
        end if
    end subroutine
end module

program ffinal513a1
use m1
    interface
        subroutine testPoint (p)
        use m1
            class (point), intent(out) :: p
        end subroutine
    end interface

    type (point3D), target :: p3d

    class (point), pointer :: p_ptr

    p3d = point3D (1.0, 2.0, 3.0)

    p_ptr => p3d

    if (p_ptr%goodPart() /= 3) error stop 1_4

    call testPoint(p_ptr)

    if (p_ptr%goodPart() /= 0) error stop 2_4

    p3d = point3D (1.0, 2.0, 3.0)

    call testPoint (p3d)

    if (p3d%goodPart() /= 0) error stop 3_4

    p3d = point3D (1.0, 2.0, 3.0)

    call testPoint (p3d%point)

    if (p_ptr%goodPart() /= 1) error stop 4_4
end

subroutine testPoint (p)
use m1
    class (point), intent(out) :: p

    if (p%goodPart() /= 0) error stop 10_4
end subroutine
