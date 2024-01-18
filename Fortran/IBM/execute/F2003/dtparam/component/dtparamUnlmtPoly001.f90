!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/18/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Unlimited poly components with
!                               parameterized derived type dynamic type; scalar
!                               components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k)
        integer, kind :: k

        real(k) :: x, y
    end type

    type, extends(point) :: point3D
        real(k) :: z
    end type

    type color (k)
        integer, kind :: k

        integer(k) :: colorVal
    end type

    type, extends(point3D) :: colorPoint3D (ck)
        integer, kind :: ck

        type (color(ck)) :: color
    end type

    interface printPoint
        module procedure printPoint4
        module procedure printPoint8
    end interface

    contains

    subroutine printPoint4 (p)
        class(point(4)), intent(in) :: p

        select type (p)
            type is (point(4))
                write (*, '(2f10.2)') p

            type is (point3D(4))
                write (*, '(3f10.2)') p

            type is (colorPoint3D(4, 1))
                write (*, '(3f10.2, i3)') p

            type is (colorPoint3D(4, 2))
                write (*, '(3f10.2, i6)') p

            class default
                stop 10
        end select
    end subroutine

    subroutine printPoint8 (p)
        class(point(8)), intent(in) :: p

        select type (p)
            type is (point(8))
                write (*, '(2f15.5)') p

            type is (point3D(8))
                write (*, '(3f15.5)') p

            type is (colorPoint3D(8, 1))
                write (*, '(3f15.5, i3)') p

            type is (colorPoint3D(8, 2))
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

    allocate (x1, source=point(4)(1.0, 2.0))
    allocate (x2, source=point(8)(1.1d1, 2.1d1))

    co1(1) = container(x2, x1)

    allocate (co1(3)%data1,source=point3D(4)(3.0, 4.0, 5.0))
    allocate (co1(3)%data2,source=point3D(8)(3.2d1, 4.3d1, 5.4d1))

    allocate (co1(5)%data1, source=colorPoint3D(4,1)(6.0, 7.0, 8.0, &
                color(1)(10)))

    allocate (co1(5)%data2, source=colorPoint3D(4,2)(9.0, 8.0, 7., &
                color(2)(367)))

    allocate (co1(7)%data1, source=colorPoint3D(8,2)(6.5d1, 7.6d1, 8.7d1, &
                color(2)(5000)))

    allocate (co1(7)%data2, source=colorPoint3D(8,1)(9.8d1, 1.09d2, 1.21d2, &
                color(1)(12)))

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
            class is (point(4))
                call printPoint (x)

            class is (point(8))
                call printPoint (x)

            class default
                stop 30
        end select
    end subroutine
end
