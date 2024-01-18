! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg510a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg510a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : dummy-arg (poly-dummy-arg in the operator
!*                               interface declaration; also tests the floating
!*                               point algorithm to ensure the precison; also
!*                               tests the elemental function for arrays)
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

program fArg510a
use m1
    type (point(4)) :: p1 (10), p2(10)
    type (point3D(4)) :: p3d_1 (3), p3d_2 (3)

    real*4 rand1(20), rand2(9)

    data p1 /point(4)(1.1, 2.1), point(4)(3.1, 4.1), point(4)(5.1, 6.1), point(4)(7.1,8.1), &
             point(4)(9.1, 8.1), point(4)(7.1, 6.1), point(4)(5.1, 4.1), point(4)(3.1, 2.1),&
             point(4)(1.1, 0.1), point(4) (1.1, 1.1)/

    data p3d_1 /point3D(4)(1.3, 2.3, 3.3), point3D(4)(4.3, 5.3, 6.3), &
                point3D(4) (7.3, 8.3, 0.3) /

    call random_number (rand1)
    call random_number (rand2)

    do i = 1, 10
        p2(i)%x = p1(i)%x * (1.0 + rand1(2*i-1)*1.e-7)
        p2(i)%y = p1(i)%y + p1(i)%y*rand1(2*i)*2.e-7
    end do

    do i = 1, 10
        if (.not. (p2(i) == p1(i))) error stop 1_4
    end do

    !! if it passes the previous check point; it should work here
    if (.not. all (p1 == p2)) error stop 2_4

    do i = 1, 3
        p3d_2(i)%x = p3d_1(i)%x + rand2(3*i-2) - rand2(3*i-2)
        p3d_2(i)%y = p3d_1(i)%y
        p3d_2(i)%z = p3d_1(i)%z * (rand2(3*i) + 0.1) / (0.1 + rand2(3*i))
    end do

    do i = 1, 3
        if (.not. (p3d_2(i) == p3d_1(i))) error stop 3_4
    end do

    !! if it passes the previous check point; it should work here
    if (.not. all (p3d_2 == p3d_1)) error stop 4_4

    !! takes a good chance that rand2(2::3) not all < 3.3e-9
    p3d_2%y = p3d_1%y + rand2(2::3) * 1.e3

    if (all (p3d_2 == p3d_1)) error stop 5_4
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

