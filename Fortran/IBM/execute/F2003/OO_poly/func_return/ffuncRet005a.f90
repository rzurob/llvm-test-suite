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
! %GROUP: ffuncRet005a.f
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
!*  DATE                       : 08/18/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : poly-function-return (use of abstract type as
!                               the function return; use pointers; a small
!                               program using shape class hierarchy)
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
    type, abstract :: shape
        contains

        procedure(genShape), pass(s), deferred :: genShape
        procedure(getArea), deferred :: area
    end type

    interface
        class(shape) function genShape (s)
            import shape
            class (shape), intent(in) :: s
            pointer genShape
        end function

        real(4) function getArea (s)
            import shape
            class (shape), intent(in) :: s
        end function
    end interface

    type point
        real(4) :: x, y
    end type
end module


module m1
use m
    type, extends(shape) :: circle
        real(4) :: radius = 1.0
        type(point) :: center = point(0,0)

        contains

        procedure :: genShape => genCirclePtr
        procedure :: area => circleArea
    end type

    contains

    class(shape) function genCirclePtr (s)
        class (circle), intent(in) :: s
        pointer genCirclePtr

        allocate (genCirclePtr, source=s)
    end function

    real(4) function circleArea (s)
        class (circle), intent(in) :: s

        real(4), parameter :: pi = 3.141593

        circleArea = pi * s%radius * s%radius
    end function
end module

program ffuncRet005a
use m1
    class(shape), pointer :: s1

    type(circle) :: c1

    logical precision_r4

    s1 => c1%genShape()

    if (.not. precision_r4(s1%area(), 3.141593)) error stop 1_4

    deallocate (s1)

    c1 = circle (radius=10.0)

    s1 => c1%genShape()

    if (.not. precision_r4(s1%area(), 3.141593*100.0)) error stop 2_4

    deallocate (s1)
end
