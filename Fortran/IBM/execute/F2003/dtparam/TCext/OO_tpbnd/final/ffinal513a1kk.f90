! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal513a1kk
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal513a1 by Jim Xia)
!*  DATE                       : 2007-10-12 (original: 04/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf90)
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
    type point (kpoint_1) ! kpoint_1=4
       integer, kind :: kpoint_1
        real(kpoint_1) :: x, y

        contains

        procedure :: goodPart => numOfGoodCompPoint
        final :: invalidatePoint
    end type

    contains

    subroutine invalidatePoint (p)
        type (point(4)), intent(inout) :: p ! tcx: (4)

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
        class (point(4)), intent(in) :: p ! tcx: (4)

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
    type, extends (point) :: point3D (kpoint3D_1) ! kpoint3D_1=4
       integer, kind :: kpoint3D_1
        real(kpoint3D_1) :: z

        contains

        procedure :: goodPart => numOfGoodCompPoint3D
        final :: invalidatePoint3D
    end type

    contains

    integer*4 function numOfGoodCompPoint3D (p)
        class (point3D(4,4)), intent (in) :: p ! tcx: (4,4)

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
        type (point3D(4,4)), intent(inout) :: p3 ! tcx: (4,4)

        if (ieee_support_sqrt(-1.0)) then
            p3%z = sqrt(-1.0)
        else
            p3%z = -1.0
        end if
    end subroutine
end module

program ffinal513a1kk
use m1
    interface
        subroutine testPoint (p)
        use m1
            class (point(4)), intent(out) :: p ! tcx: (4)
        end subroutine
    end interface

    type (point3D(4,4)), target :: p3d ! tcx: (4,4)

    class (point(4)), pointer :: p_ptr ! tcx: (4)

    p3d = point3D(4,4) (1.0, 2.0, 3.0) ! tcx: (4,4)

    p_ptr => p3d

    if (p_ptr%goodPart() /= 3) error stop 101_4

    call testPoint(p_ptr)

    if (p_ptr%goodPart() /= 0) error stop 2_4

    p3d = point3D(4,4) (1.0, 2.0, 3.0) ! tcx: (4,4)

    call testPoint (p3d)

    if (p3d%goodPart() /= 0) error stop 3_4

    p3d = point3D(4,4) (1.0, 2.0, 3.0) ! tcx: (4,4)

    call testPoint (p3d%point)

    if (p_ptr%goodPart() /= 1) error stop 4_4
end

subroutine testPoint (p)
use m1
    class (point(4)), intent(out) :: p ! tcx: (4)

    if (p%goodPart() /= 0) error stop 10_4
end subroutine


! Extensions to introduce derived type parameters:
! type: point - added parameters (kpoint_1) to invoke with (4) / declare with (4) - 5 changes
! type: point3D - added parameters (kpoint3D_1) to invoke with (4,4) / declare with (4,4) - 6 changes
