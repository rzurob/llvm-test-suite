! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/29/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statement: An extended type has a scalar,
!                               nonpointer, nonallocatable parent component with
!                               the type and type parameters of the parent type.
!                               Case: use the parent components in the intrinsic
!                               assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(k)
        integer, kind :: k

        real(k) :: x, y
    end type

    type color (k)
        integer, kind :: k

        integer(k) :: RGB(3)
    end type

    type, extends(point) :: colorPoint(colorKind)
        integer, kind :: colorKind

        type(color(colorKind)) color
    end type
end module


program dtparamExtends018
use m
    type (point(8)) p1(2)

    type (colorPoint(8, colorKind=2)) cp1(10)

    logical(4) precision_r8

    cp1%x = (/(i*1.0d0, i=1,10)/)
    cp1%y = (/(i*3.3d0, i=1,10)/)

    !! get blue color for all points: 0:0:255
    cp1%color%RGB(1) = 0
    cp1%color%RGB(2) = 0
    cp1%color%RGB(3) = 255

    p1 = cp1(2:3)%point

    if ((.not. precision_r8(p1(1)%x, 2*1.0d0)) .or. &
        (.not. precision_r8(p1(1)%y, 2*3.3d0))) error stop 1_4


    if ((.not. precision_r8(p1(2)%x, 3*1.0d0)) .or. &
        (.not. precision_r8(p1(2)%y, 3*3.3d0))) error stop 2_4

end
