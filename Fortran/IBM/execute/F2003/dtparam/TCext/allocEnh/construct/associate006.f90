! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/construct/associate006.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/11/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the case where multiple associations in the
!                               same associate construct.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)      :: x, y

        contains

        procedure :: rotate => rotateAroundZero
        procedure :: length => lengthBetweenPoints
    end type

    contains

    elemental type(point(20,4)) function rotateAroundZero (b, p)
        class(point(*,4)), intent(in) :: b
        real, intent(in) :: p

        real theta, radius

        !! calculate the phase first
        theta = atan2(b%y, b%x) + p
        radius = sqrt(b%x**2 + b%y**2)

        rotateAroundZero = point(20,4)(radius*cos(theta), radius*sin(theta))
    end function


    elemental real function lengthBetweenPoints (p1, p2)
        class(point(*,4)), intent(in) :: p1, p2

        lengthBetweenPoints = sqrt((p1%x-p2%x)**2 + (p1%y-p2%y)**2)
    end function

    logical function pointEqual (p1, p2)
        type(point(*,4)), intent(in) :: p1, p2

        real, parameter :: tol = 1.e-6
        logical(4), external :: precision_r4

        if ((abs(p1%x-p2%x) < tol) .and. (abs(p1%y-p2%y) < tol)) then
            pointEqual = .true.
        else
            pointEqual = precision_r4(p1%x, p2%x) .and. precision_r4(p1%y, p2%y)
        end if

    end function
end module

program associate006
use m
    type(point(:,4)), allocatable :: p1(:), p2

    real, allocatable :: r1(:)

    real, parameter :: pi = 3.14159265

    logical(4), external :: precision_r4

    p2 = point(20,4)(1.5, 1.5)

    associate (x => p2%rotate((/(pi/4*i, i=1,7)/)), y => p2%rotate(2*pi))
        p1 = x

        associate (z => p1%rotate(2*pi))
            r1 = z%length(y)
        end associate
    end associate

    if ((.not. allocated(p1)) .or. (.not. allocated(r1))) error stop 1_4

    do i = 1, 7
        if (.not. pointEqual(p1(i), p2%rotate(pi*i/4.0))) error stop 2_4
    end do

    if ((.not. precision_r4 (3.0_4, r1(2))) .or. &
        (.not. precision_r4 (3.0_4, r1(6)))) error stop 3_4

    if (.not. precision_r4 (3*sqrt(2.0), r1(4))) error stop 4_4

    if ((.not. precision_r4 (sqrt(4.0-2*sqrt(2.0))*1.5, r1(1))) .or. &
        (.not. precision_r4 (sqrt(4.0-2*sqrt(2.0))*1.5, r1(7)))) error stop 5_4

    if ((.not. precision_r4 (sqrt(4.0+2*sqrt(2.0))*1.5, r1(3))) .or. &
        (.not. precision_r4 (sqrt(4.0+2*sqrt(2.0))*1.5, r1(5)))) error stop 6_4
end
