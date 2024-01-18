!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2005
!*
!*  DESCRIPTION                : specific type bound (select type usage in the
!                               type bound)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        integer(8) x, y

        contains

        procedure :: print => printPoint
    end type

    type, extends(point) :: colorPoint
        integer(selected_int_kind(3)) color
    end type

    contains

    subroutine printPoint (p)
        class (point), intent(in) :: p

        select type (p)
            type is (point)
                print *, p%x, p%y
            class default
                print *, 'print operation not defined'
        end select
    end subroutine
end module

program ftpbnd522
use m
    class (point), allocatable :: p1(:)

    allocate (p1(3), source=(/colorPoint(10, 20, 2), colorPoint(30, 40, 4), &
                        colorPoint (1, 5, 2)/))

    call p1(2)%print

    select type (p1)
        type is (colorPoint)
            associate (x => p1%point)
                call x(1)%print
                call x(2)%print
                call x(3)%print
            end associate
        class default
            error stop 1_4
    end select
end
