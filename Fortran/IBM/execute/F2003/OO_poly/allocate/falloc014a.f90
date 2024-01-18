!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc014a.f
! %VERIFY: falloc014a.out:falloc014a.vf
! %STDIN:
! %STDOUT: falloc014a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (allocatable array component
!                               allocation or deallocation during intrinsic
!                               assignment; focus on bounds and type)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        real(4) :: x, y
    end type

    type, abstract :: shape
        contains

        procedure(printShape), deferred, pass(s) :: print
    end type

    type, extends(shape) :: circle
        type(point) :: center
        real(4) :: radius = 1.0

        contains

        procedure :: print => printCircle
    end type

    interface
        subroutine printShape (s)
        import shape
            class (shape), intent(in) :: s
        end subroutine
    end interface

    contains

    subroutine printCircle (s)
        class (circle), intent(in) :: s

        write (*, '(a17,2f15.2)') "circle's center:", s%center%x, s%center%y
        write (*, '(a17,f15.2)') "circle's radius:", s%radius
    end subroutine
end module

module m1
use m
    type plate
        class (shape), allocatable :: shapes(:)

        contains

        procedure :: print => printPlate
    end type

    contains

    subroutine printPlate (p)
        class (plate), intent(in) :: p

        if (allocated (p%shapes)) then
            do i = lbound(p%shapes,1), ubound(p%shapes,1)
                call p%shapes(i)%print
            end do
        else
            print *, 'there is no data to print'
        end if
    end subroutine
end module

program falloc014a
use m1
    type (plate) :: p1, p2

    allocate (p2%shapes(-1:1), source=(/(circle(point(i, 0)), i = -1,1)/))

    !! intrinsic assignment involves the data components' allocation
    p1 = p2

    if ((lbound(p1%shapes,1) /= -1) .or. (ubound(p1%shapes,1) /= 1)) error stop 1_4

    call p1%print

    deallocate (p2%shapes)

    if (.not. allocated(p1%shapes)) error stop 1_4

    !! intrinsic assignment involves deallocation of allocated allocatable
    !component
    p1 = p2

    if (allocated (p1%shapes)) error stop 2_4

    call p1%print
end
