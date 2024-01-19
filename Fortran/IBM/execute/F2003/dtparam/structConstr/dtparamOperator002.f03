! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/21/2006
!*
!*  DESCRIPTION                : dtparam (scetion 4.5.9: structure constructor)
!                               Case: Unary and binary operator (-) defined for
!                               the same derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k)
        integer, kind :: k

        real(k) :: x, y
    end type
end module

module n
    interface
        type(point(8)) function negate8 (p1)
        use m
            type(point(8)), intent(in) :: p1
        end function

        type(point(4)) function minus (p1, p2)
        use m
            type(point(4)), intent(in) :: p1, p2
        end function
    end interface
end module

program dtparamOperator002
use m
use n
    interface operator (-)
        procedure negate8
        procedure minus
    end interface

    type(point(8)) :: p8_1(10)
    type(point(4)), allocatable :: p4_1(:)

    logical(4), external :: precision_r4, precision_r8

    do i = 1, 5
        p8_1(i) = -point(8)(i-1, -log(1.0d0*i))
    end do

    do i = 6, 10
        p8_1(i) = -p8_1(11-i)
    end do

    allocate (p4_1(10))

    p4_1(1:3:2) = (/point(4)(2,2), point(4)(3,1)/)

    p4_1(2) = p4_1(1) - p4_1(3)


    !! verify p4_1(2) and p8_1
    if (.not. precision_r4(p4_1(2)%x, -1.0)) error stop 1_4

    if (.not. precision_r4(p4_1(2)%y, 1.0)) error stop 2_4

    do i = 1, 5
        if (.not. precision_r8(p8_1(i)%x, -1.0d0*(i-1))) error stop 3_4

        if (.not. precision_r8(p8_1(i)%y, log(1.0d0*i))) error stop 4_4
    end do

    do i = 6, 10
        if (.not. precision_r8(p8_1(i)%x, 1.0d0*(10-i))) error stop 5_4

        if (.not. precision_r8(p8_1(i)%y, -log(1.0d0*(11-i)))) error stop 6_4
    end do
end

type(point(8)) function negate8 (p1)
use m
    type(point(8)), intent(in) :: p1

    negate8 = point(8)(-p1%x, -p1%y)
end function

type(point(4)) function minus (p1, p2)
use m
    type(point(4)), intent(in) :: p1, p2

    minus = point(4) (x=p1%x-p2%x, y=p1%y-p2%y)
end function
