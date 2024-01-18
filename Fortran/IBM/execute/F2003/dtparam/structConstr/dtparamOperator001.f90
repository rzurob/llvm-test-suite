!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/21/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Unary operator (-) for derived type point.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k)
        integer, kind :: k

        real(k) :: x, y
    end type

    interface operator(-)
        module procedure negate4
        module procedure negate8
    end interface

    contains

    type(point(4)) function negate4 (p1)
        type(point(4)), intent(in) :: p1

        negate4%x = -p1%x
        negate4%y = -p1%y
    end function

    type(point(8)) function negate8 (p1)
        type(point(8)), intent(in) :: p1

        negate8 = point(8)(-p1%x, -p1%y)
    end function
end module

program dtparamOperator001
use m
    class(point(8)), allocatable :: p8_1(:)

    type (point(4)) :: p4_1

    p4_1 = -point(4)(1.0, -2.1)

    allocate (p8_1(10), source=(/(-point(8)(i, -i), i=1,10)/))

    write(*, '(2f10.2)') p4_1
    write(*, '(10(2f15.5, ";"))') (p8_1(i)%x, p8_1(i)%y, i=1, 10)
end
