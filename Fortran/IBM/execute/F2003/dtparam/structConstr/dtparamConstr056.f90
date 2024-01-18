! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/18/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Test the dynamic type of the allocated
!                               allocatable component that is declared to be
!                               polymorphic.  Component is scalar.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k, dim)
        integer, kind :: k
        integer, len :: dim

        real(k) :: x(dim)

        contains

        procedure :: print4 => printPoint4
    end type

    integer BLACK, RED, BLUE
    parameter (BLACK=0, RED=1, BLUE=2)

    character(5) :: colorVal(0:2)
    parameter (colorVal = (/'BLACK', 'RED  ', 'BLUE '/))

    type, extends(point) :: colorPoint (ck)
        integer, kind :: ck

        integer(ck) :: color = BLACK

        contains

        procedure :: print4 => printColorPoint42
    end type

    contains

    subroutine printPoint4 (p)
        class(point(4,*)), intent(in) :: p

        do i = 1, p%dim
            write (*, '(1x, f10.3)', advance='no') p%x(i)
        end do
    end subroutine

    subroutine printColorPoint42 (p)
        class(colorPoint(4,*,2)), intent(in) :: p

        call p%point%print4

        print *, 'color = ', colorVal(p%color)
    end subroutine
end module


module n
use m
    type node (k)
        integer, kind :: k

        class(point(k,:)), allocatable :: data
        type(node(k)), pointer :: next => null()
    end type
end module

program dtparamConstr056
use n
    type(node(4)), pointer :: list, iterator

    class(point(4,:)), allocatable :: p1, p2(:)
    type(point(4,3)) p3(5)
    type(colorPoint(4,2,2)) :: cp1


    allocate (p1, source=point(4,4)(x=(/(i*1.1_4, i=1,4)/)))

    allocate (p2(50), source=colorPoint(4,2,2)((/-1.0, 1.2/), BLUE))

    p3 = (/(point(4,3)((/(j, j=i-2,i)/)), i=1, 5)/)

    cp1 = colorPoint(4,2,2)(10, RED)

    allocate (list)

    list = node(4)(p1)

    allocate (list%next, source=node(4)(p2(35), null()))

    allocate (list%next%next)
    list%next%next = node(4)(data=p3(2), next=null())

    allocate (list%next%next%next, source=node(4)(cp1))


    !! now print out the list
    iterator => list

    do while (associated(iterator))
        if (.not. allocated(iterator%data)) stop 2

        call iterator%data%print4
        iterator => iterator%next
    end do

    deallocate (list%next%next%next)
    deallocate (list%next%next)
    deallocate (list%next)
    deallocate (list)
end
