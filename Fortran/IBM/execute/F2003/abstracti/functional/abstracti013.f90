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
! %GROUP: ffuncRet005a1.f
! %VERIFY: ffuncRet005a1.out:ffuncRet005a1.vf
! %STDIN:
! %STDOUT: ffuncRet005a1.out
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
!*  PROGRAMMER                 : Alberto Alvarez-Mesquida
!*  DATE                       : 02/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : poly-function-return (function returns
!                               allocatable of a class of abstract type SHAPE)
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
        real(4) :: x = 0.0
        real(4) :: y = 0.0
    end type

    type, abstract :: shape
        contains

        procedure(genShape), pass(s), deferred :: genShape
        procedure(getArea), deferred :: area
    end type

    abstract interface
        class(shape) function genShape (s)
            import shape
            class (shape), intent(in) :: s
            allocatable genShape
        end function

        real(4) function getArea (s)
            import shape
            class (shape), intent(in) :: s
        end function
    end interface

    contains

    subroutine printArea (s)
        class (shape), intent(in) :: s

        print *, s%area()
    end subroutine
end module


module m1
use m
    type, extends(shape) :: triangle
        type(point) :: vertices(3)

        contains

        procedure :: area => getTriArea
        procedure :: genShape => genTriangle
        final :: finalizeTriangle
    end type

    contains

    class (shape) function genTriangle (s)
        class (triangle), intent(in) :: s
        allocatable genTriangle

        allocate (genTriangle, source=s)
    end function

    real(4) function getTriArea (s)
        class (triangle), intent(in) :: s

        !! for this triangle area computation we use vertices and determinant
        !of a matrix

        real(8) :: matrix(3,3), s1

        matrix(:,3) = 1.0d0
        matrix(:,2) = s%vertices%y
        matrix(:,1) = s%vertices%x

        !! the area is the determinant(matrix) / 2
        s1 = matrix(1,1) * (matrix(2,2)*matrix(3,3) - matrix(3,2)*matrix(2,3))&
            -matrix(1,2) * (matrix(2,1)*matrix(3,3) - matrix(3,1)*matrix(2,3)) &
            +matrix(1,3) * (matrix(2,1)*matrix(3,2) - matrix(3,1)*matrix(2,2))

        getTriArea = abs(s1)/2.0d0
    end function

    subroutine finalizeTriangle (t)
        type (triangle), intent(inout) :: t

        print *, 'finalizeTriangle'
    end subroutine
end module

program abstracti013
use m1
    type (triangle), save :: t1
    class (shape), allocatable :: s1

    t1 = triangle ((/point(1,1),point(2,2), point(3.0,1)/))

    call printArea(t1%genShape())

    allocate (s1, source=t1)

    call printArea (s1)

    deallocate(s1)
end
