!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/16/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (An extended case from
!                               dtpPass002 with inherited binding and overridden
!                               binding.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (dim)
        integer, len :: dim

        real coor(dim)

        contains

        procedure :: print => printPoint
        procedure, non_overridable :: direction
    end type

    contains

    subroutine printPoint (p1)
        class(point(*)), intent(in) :: p1

        write (*, '(5g12.5)') p1%coor
    end subroutine

    function direction (p1)
        class(point(*)), intent(in) :: p1

        type(point(p1%dim)) direction

        real scale1

        scale1 = sqrt(sum (p1%coor**2))

        direction%coor = p1%coor / scale1
    end function
end module


module m1
use m, only: point

    enum, bind(c)
        enumerator :: RED = 1, BLUE, GREEN, BLACK
    end enum

    character(5), parameter :: color(4) = [character(5) :: &
        'RED', 'BLUE', 'GREEN', 'BLACK']

    type, extends(point) :: colorPoint (ck)
        integer, kind :: ck = 4

        integer(ck) :: color = RED

        contains

        procedure :: print => printColorPoint
    end type

    interface
        subroutine printColorPoint (p1)
        import
            class(colorPoint(*)), intent(in) :: p1
        end subroutine
    end interface
end module


subroutine printColorPoint (p1)
use m1, only: colorPoint, color
    class(colorPoint(*)), intent(in) :: p1

    write (*, '(5g12.5)', advance='no') p1%coor
    write (*, '(1x, a5)') color(p1%color)
end subroutine


program dtpPass002a
use m1
    class (point(:)), pointer :: p1(:,:)

    type (colorPoint(3,4)), target :: cp(4, 2)

    type(point(:)), allocatable :: p2

    cp(:,1)%color = [red, BLUE, GREEN, BLACK]
    cp(:,2)%color = cp(:,1)%color

    do j = 1, 2
        do i = 1, 4
            cp(i, j)%coor = [real:: i, j, sqrt(real(i**2+j**2))]
        end do
    end do

    ! point p1 to cp
    p1 => cp

    do i = 1, 4
        do j = 1, 2
            call p1(i,j)%print

            associate (x => p1(i,j)%direction())
                write (*, '(A)', advance='no') 'direction is: '

                call x%print
            end associate
        end do
    end do

    !! allocate p1
    allocate (point(2) :: p1(3,3))

    do i = 1, 3
        do j = 1, 3
            p1(i,j)%coor = [i, j]
        end do
    end do

    do j = 1, 3
        do i = 1, 3
            call p1(i,j)%print

            p2 = p1(i,j)%direction()

            write (*, '(A)', advance='no') 'direction is: '

            call p2%print
        end do
    end do
end
