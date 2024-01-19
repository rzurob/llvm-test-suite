! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/20/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: parameterized programming: shape class <--
!                               circle and polygon.  Also use the type bound
!                               generics.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module shapeMod
    type point (k)
        integer, kind :: k

        real(k) :: x, y
    end type

    type, abstract :: shape (k)
        integer, kind :: k = 4

        contains

        generic :: print => print4, print8
        procedure(printShape4), deferred :: print4
        procedure(printShape8), deferred :: print8
    end type

    abstract interface
        subroutine printShape4(s)
        import
            class(shape(4)), intent(in) :: s
        end subroutine

        subroutine printShape8(s)
        import
            class(shape(8)), intent(in) :: s
        end subroutine
    end interface
end module


module geometry
use shapeMod, only : point, shape

    type, extends(shape) :: circle
        type(point(k)) center
        real (k) radius

        contains

        procedure :: print4 => printCircle4
        procedure :: print8 => printCircle8
    end type

    type, extends(shape) :: polygon (n)
        integer, len :: n = 3

        type(point(k)) vertices(n)

        contains

        procedure :: print4 => printPolygon4
        procedure :: print8 => printPolygon8
    end type

    contains

    subroutine printCircle4 (s)
        class(circle), intent(in) :: s

        print *, 'Type Circle, kind 4'
        write (*, '(a,2f10.2)') "Center: ", s%center
        write (*, '(a, f10.2)') "Radius: ", s%radius
    end subroutine

    subroutine printCircle8 (s)
        class(circle(8)), intent(in) :: s

        print *, 'Type Circle, kind 8'
        write (*, '(a,2g10.2)') "Center: ", s%center
        write (*, '(a, g10.2)') "Radius: ", s%radius
    end subroutine

    subroutine printPolygon4 (s)
        class(polygon(4, *)), intent(in) :: s

        print *, 'Type Polygon, kind 4, size = ', s%n

        do i = 1, s%n
            write (*, '(a, i3, 2f10.2)') 'Vertex', i, s%vertices(i)
        end do
    end subroutine

    subroutine printPolygon8 (s)
        class(polygon(8, *)), intent(in) :: s

        print *, 'Type Polygon, kind 8, size = ', s%n

        do i = 1, s%n
            write (*, '(a, i3, 2g10.2)') 'Vertex', i, s%vertices(i)
        end do
    end subroutine
end module


program dtparamExtends022a
use geometry

    type shapeNode(k)
        integer, kind :: k = 4

        type (shapeNode(k)), pointer :: next => null()
        class(shape(k)), allocatable :: data
        class(shape(2*k)), allocatable :: data2
    end type

    type(shapeNode), pointer :: list
    type(shapeNode), pointer :: iterator

    allocate (list)

    !!!----
    ! now add 4 nodes to the list
    ! 1st is a circle of kind 4, center (1.0, 2.0) radius = 12.1
    allocate (list%data, source=circle(center=point(4)(1.0, 2.0), &
            radius=1.21e1))

    !! 2nd is a square of kind 8: (0, 0), (10, 0), (10, 10), (0, 10)
    allocate (list%next)
    allocate (list%next%data2, source=polygon(8, 4)((/point(8)(0.0d0, 0.0d0), &
        point(8)(1.0d1, 0.0d0), point(8)(1.0d1, 1.0d1), point(8)(0, 1.0d1)/)))


    !! the 3rd is a polygon of kind 4: (0,0), (10,0), (10,5), (5,10), (0,5)
    allocate (list%next%next)
    allocate (list%next%next%data, source=polygon(n=5)((/point(4)(0.e0, 0.e0),&
        point(4)(1.e1, 0.e0), point(4)(1.e1, .5e1), point(4)(.5e1, 1.e1), &
        point(4)(0.e0, .5e1)/)))


    !! the 4th element is a circleof kind 8: center: (5,0), radius=5.0
    allocate (list%next%next%next)
    allocate (list%next%next%next%data2, source=circle(8)(point(8)(.5e1,0.e0),&
            .5e1))

    !! now print the list
    iterator => list

    do while (associated (iterator))
        if (allocated(iterator%data)) call iterator%data%print
        if (allocated(iterator%data2)) call iterator%data2%print

        iterator => iterator%next
    end do
end
