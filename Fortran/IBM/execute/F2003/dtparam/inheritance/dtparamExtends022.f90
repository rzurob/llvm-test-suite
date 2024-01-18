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
!*  DATE                       : 12/12/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: Abstract type with type parameters: type
!                               shape and its extended types; also a linked-list
!                               to work with.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module shapeMod
    type point (k)
        integer, kind :: k = 4

        real(k) :: x, y
    end type

    type, abstract :: shape (k)
        integer, kind :: k = 4

        contains

        procedure(printShape), deferred :: print
    end type

    abstract interface
        subroutine printShape(s)
        import
            class(shape), intent(in) :: s
        end subroutine
    end interface
end module


module geometry
use shapeMod, only : point, shape

    type, extends(shape) :: circle
        type(point(k)) center
        real (k) radius

        contains

        procedure :: print => printCircle
    end type

    type, extends(shape) :: polygon (n)
        integer, len :: n = 3

        type(point(k)) vertices(n)

        contains

        procedure :: print => printPolygon
    end type

    contains

    subroutine printCircle (s)
        class(circle), intent(in) :: s

        print *, 'Type Circle'
        write (*, '(a,2f10.2)') "Center: ", s%center
        write (*, '(a, f10.2)') "Radius: ", s%radius
    end subroutine

    subroutine printPolygon (s)
        class(polygon(n=*)), intent(in) :: s

        print *, 'Type Polygon ', s%n

        do i = 1, s%n
            write (*, '(a, i5, 2f10.2)') 'Vertex', i, s%vertices(i)
        end do
    end subroutine
end module


program dtparamExtends022
use geometry

    type shapeNode(k)
        integer, kind :: k = 4

        type (shapeNode(k)), pointer :: next => null()
        class(shape(k)), allocatable :: data
    end type

    type(shapeNode), pointer :: list
    type(shapeNode), pointer :: iterator

    allocate (list)

    allocate (list%data, source = polygon ((/point(1.0, 2.0), point(2.0, 3.0), &
                point(3.0, 0.0)/)))

    allocate (list%next)
    allocate (list%next%data, source=circle(center=point(1.0, 1.0), &
                radius=2.0))

    allocate (list%next%next)
    allocate (list%next%next%data, source=circle(radius= 1.0, center=point(0.0,&
                1.0)))

    !! now print the list
    iterator => list

    do while (associated (iterator))
        if (allocated(iterator%data)) call iterator%data%print

        iterator => iterator%next
    end do
end
