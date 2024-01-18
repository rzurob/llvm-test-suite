!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 01/18/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 315110.2)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point4! (k)
        real(4) :: x, y
    end type

    type point8! (k)
        real(8) :: x, y
    end type

    type, extends(point4) :: point3D4
        real(4) :: z
    end type

    type, extends(point8) :: point3D8
        real(8) :: z
    end type

    type color1! (k)
        integer(1) :: colorVal
    end type

    type color2! (k)
        integer(2) :: colorVal
    end type

    type, extends(point3D4) :: colorPoint3D4_1! (ck)
        type (color1) :: color
    end type

    type, extends(point3D4) :: colorPoint3D4_2! (ck)
        type (color2) :: color
    end type

    type, extends(point3D8) :: colorPoint3D8_1! (ck)
        type (color1) :: color
    end type

    type, extends(point3D8) :: colorPoint3D8_2! (ck)
        type (color2) :: color
    end type

    interface printPoint
        module procedure printPoint4
        module procedure printPoint8
    end interface

    contains

    subroutine printPoint4 (p)
        class(point4), intent(in) :: p

        select type (p)
            type is (point4)
                write (*, '(2f10.2)') p

            type is (point3D4)
                write (*, '(3f10.2)') p

            type is (colorPoint3D4_1)
                write (*, '(3f10.2, i3)') p

            type is (colorPoint3D4_2)
                write (*, '(3f10.2, i6)') p

            class default
                stop 10
        end select
    end subroutine

    subroutine printPoint8 (p)
        class(point8), intent(in) :: p

        select type (p)
            type is (point8)
                write (*, '(2f15.5)') p

            type is (point3D8)
                write (*, '(3f15.5)') p

            type is (colorPoint3D8_1)
                write (*, '(3f15.5, i3)') p

            type is (colorPoint3D8_2)
                write (*, '(3f15.5, i6)') p

            class default
                stop 20
        end select
    end subroutine
end module

program dtparamUnlmtPoly001
use m
    type container
        class(*), allocatable :: data1
        class(*), pointer :: data2 => null()
    end type

    type (container) co1(30)

    class(*), pointer :: x1
    class(*), allocatable :: x2

    allocate (x1, source=point4(1.0, 2.0))
    allocate (x2, source=point8(1.1d1, 2.1d1))

    co1(1) = container(x2, x1)

    allocate (co1(3)%data1,source=point3D4(3.0, 4.0, 5.0))
    allocate (co1(3)%data2,source=point3D8(3.2d1, 4.3d1, 5.4d1))

    allocate (co1(5)%data1, source=colorPoint3D4_1(6.0, 7.0, 8.0, &
                color1(10)))

    allocate (co1(5)%data2, source=colorPoint3D4_2(9.0, 8.0, 7., &
                color2(367)))

    allocate (co1(7)%data1, source=colorPoint3D8_2(6.5d1, 7.6d1, 8.7d1, &
                color2(5000)))

    allocate (co1(7)%data2, source=colorPoint3D8_1(9.8d1, 1.09d2, 1.21d2, &
                color1(12)))

    co1(15) = co1(1)
    co1(16:18) = co1(3:7:2)

    if (.not. associated(co1(1)%data2, x1)) error stop 5_4

    do i = 15, 18
        if (.not. allocated(co1(i)%data1)) error stop 1_4
        if (.not. associated(co1(i)%data2, co1(2*i-29)%data2)) error stop 2_4

        call printX (co1(i)%data1)
        call printX (co1(i)%data2)
    end do


    contains

    subroutine printX (x)
        class(*), intent(in) :: x

        select type (x)
            class is (point4)
                call printPoint (x)

            class is (point8)
                call printPoint (x)

            class default
                stop 30
        end select
    end subroutine
end
