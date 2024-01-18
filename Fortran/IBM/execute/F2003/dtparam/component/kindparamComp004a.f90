!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/18/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Kind type parameter and component:
!                               poly-allocatable parameterized component; array
!                               components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k)
        integer, kind :: k

        real(k) :: x, y
    end type

    type color (k)
        integer, kind :: k

        integer(k) :: colorVal
    end type
end module

module m1
use m
    type, extends(point) :: colorPoint (ck)
        integer, kind :: ck = 1

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

            type is (colorPoint(4, 1))
                write (*, '(2f10.2, i2)') p

            type is (colorPoint(4, 2))
                write (*, '(2f10.2, i4)') p

            type is (colorPoint(4, 4))
                write (*, '(2f10.2, i8)') p

            class default
                error stop 10_4
        end select
    end subroutine

    subroutine printPoint8 (p)
        class(point(8)), intent(in) :: p

        select type (p)
            type is (point(8))
                write (*, '(2f15.5)') p

            type is (colorPoint(8, 1))
                write (*, '(2f15.5, i2)') p

            type is (colorPoint(8, 2))
                write (*, '(2f15.5, i4)') p

            type is (colorPoint(8, 4))
                write (*, '(2f15.5, i8)') p

            class default
                error stop 20_4
        end select
    end subroutine
end module

module m2
use m
    type pointCollect (k)
        integer, kind :: k

        class(point(k)), allocatable :: p1
    end type
end module

program kindparamComp004
use m1
use m2
    type (pointCollect(4)) :: pc1(10)
    type (pointCollect(8)), allocatable :: pc2(:)

    allocate (pc2(20))

    !! set values for pc1
    allocate (pc1(1)%p1, source=point(4)(1.0, 2.0))
    allocate (pc1(2)%p1, source=colorPoint(4,1)(2.0, 3.0, color(1)(2)))
    allocate (pc1(3)%p1, source=colorPoint(4,2)(3.0, 4.0, color(2)(10)))
    allocate (pc1(4)%p1, source=colorPoint(4,4)(4.0, 5.0, color(4)(255)))

    pc1(5:8) = pc1(1:4)


    !! set values for pc2
    allocate (pc2(20)%p1, source=colorPoint(8,4)(1.d1, 1.2d1, color(4)(300)))
    allocate (pc2(18)%p1, source=colorPoint(8,2)(1.2d1, 2.d1, color(2)(20)))
    allocate (pc2(16)%p1, source=colorPoint(8,1)(2.d1, 3.2d1, color(1)(5)))
    allocate (pc2(14)%p1, source=point(8)(3.2d1, 4.d1))

    pc2(5:8) = pc2(14::2)

    !! verify
    do i = 5, 8
        if (.not. allocated(pc1(i)%p1)) error stop 1_4

        if (.not. allocated(pc2(i)%p1)) error stop 2_4

        call printPoint(pc1(i)%p1)
        call printPoint(pc2(i)%p1)

        deallocate (pc1(i)%p1, pc2(i)%p1)
        deallocate (pc1(i-4)%p1, pc2(2*i+4)%p1)
    end do
end
