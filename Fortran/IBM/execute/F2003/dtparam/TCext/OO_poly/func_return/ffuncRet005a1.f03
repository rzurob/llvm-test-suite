! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet005a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type point(k1)    ! (4)
        integer, kind :: k1
        real(k1)      :: x = 0.0
        real(k1)      :: y = 0.0
    end type

    type, abstract :: shape(k2,n1)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n1
        contains

        procedure(genShape), pass(s), deferred :: genShape
        procedure(getArea), deferred :: area
    end type

    interface
        class(shape(4,:)) function genShape (s)
            import shape
            class (shape(4,*)), intent(in) :: s
            allocatable genShape
        end function

        real(4) function getArea (s)
            import shape
            class (shape(4,*)), intent(in) :: s
        end function
    end interface

    contains

    subroutine printArea (s)
        class (shape(4,*)), intent(in) :: s

        print *, s%area()
    end subroutine
end module


module m1
use m
    type, extends(shape) :: triangle    ! (4,20)
        type(point(k2)) :: vertices(3)

        contains

        procedure :: area => getTriArea
        procedure :: genShape => genTriangle
        final :: finalizeTriangle
    end type

    contains

    class (shape(4,:)) function genTriangle (s)
        class (triangle(4,*)), intent(in) :: s
        allocatable genTriangle

        allocate (genTriangle, source=s)
    end function

    real(4) function getTriArea (s)
        class (triangle(4,*)), intent(in) :: s

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
        type (triangle(4,*)), intent(inout) :: t

        print *, 'finalizeTriangle'
    end subroutine
end module

program ffuncRet005a1
use m1
    type (triangle(4,20)), save :: t1
    class (shape(4,:)), allocatable :: s1

    t1 = triangle(4,20) ((/point(4)(1,1),point(4)(2,2), point(4)(3.0,1)/))

    call printArea(t1%genShape())

    allocate (s1, source=t1)

    call printArea (s1)

    deallocate(s1)
end
