! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/11/2005
!*
!*  DESCRIPTION                : type-bound procedure (private keyword on
!                               type-bound part ONLY affects type-bound part)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        real(4) x, y

        contains

        private     !<-- this keyword only affects type-bound part of this type
        procedure :: notToUse => hidden

        procedure, public :: print => printPoint
    end type

    type, extends(point) :: point3D
        real(4) :: z = 0.5

        contains

        procedure :: print => printPoint3D
    end type

    private hidden

    contains

    subroutine hidden (p)
        class(point), intent(in) :: p
    end subroutine

    subroutine printPoint (p, fmt)
        class(point), intent(in) :: p
        character(*), intent(in) :: fmt

        write (*, fmt) p%x, p%y
    end subroutine

    subroutine printPoint3D (p, fmt)
        class(point3D), intent(in) :: p
        character(*), intent(in) :: fmt

        write (*, fmt) p%x, p%y, p%z
    end subroutine
end module

program ftpbnd525
use m
    class (point), allocatable :: p1(:)
    type (point) :: p2 = point(1.0, 3.0)

    allocate(point3D:: p1(10))

    do i = 1, 10
        p1(i)%x = 1.0
        p1(i)%y = 3.0
    end do

    call p2%print("('x = ',f10.2,'; y = ', f10.2))")

    do i = 1, 10
        call p1(i)%print (fmt = '(3f10.3)')
    end do
end
