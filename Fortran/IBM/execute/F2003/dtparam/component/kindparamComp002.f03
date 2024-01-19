! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/17/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Kind type parameter and component:
!                               poly-allocatable parameterized component;
!                               scalar components and structure constructors.
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

    contains

    subroutine printPoint4 (p)
        class(point(4)), intent(in) :: p

        select type (p)
            type is (point(4))
                write (*, '(2f10.2)') p%x, p%y

            type is (point3D(4))
                write (*, '(3f10.2)') p%x, p%y, p%z

            class default
                stop 1
        end select
    end subroutine

    subroutine printPoint8 (p)
        class(point(8)), intent(in) :: p

        select type (p)
            type is (point(8))
                write (*, '(2f15.5)') p%x, p%y

            type is (point3D(8))
                write (*, '(3f15.5)') p%x, p%y, p%z

            class default
                stop 10
        end select
    end subroutine
end module

module n
use m
    type dot (k)
        integer, kind :: k

        class (point(k)), allocatable :: p1
    end type
end module

program kindparamComp002
use n
    type (dot(4)) d1
    type (dot(8)) d2

    class(point(4)), allocatable :: p1(:)
    class(point(8)), pointer :: p2

    allocate (p1(2), source=(/point(4)(1.0, 2.0), point(4)(3.0, 4.0)/))
    allocate (p2, source=point3D(8)(1.1d0, 3.1d0, 2.1d0))

    d1 = dot(4) (p1(2))
    d2 = dot(8)(p2)

    if ((.not. allocated(d1%p1)) .or. (.not. allocated(d2%p1))) error stop 1_4

    call printPoint4 (d1%p1)
    call printPoint8 (d2%p1)
end
