!***********************************************************************
!* =====================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!* =====================================================================
!*
!*  TEST CASE NAME             : abstracti010k
!*
!*  PROGRAMMER                 : Glen Mateer (derived from abstracti010
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-17 (original: 02/20/2006)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface 
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : ALLOCATE (allocatable array component
!*  allocation or deallocation during intrinsic assignment; focus on bounds
!*  and type)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!* =====================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (kpoint_1) ! kpoint_1=4
       integer, kind :: kpoint_1
        real(kpoint_1) :: x, y
    end type

    type, abstract :: shape (kshape_1) ! kshape_1=4
       integer, kind :: kshape_1
        contains

        procedure(printShape), deferred, pass(s) :: print
    end type

    type, extends(shape) :: circle (kcircle_1) ! kcircle_1=4
       integer, kind :: kcircle_1
        type(point(kshape_1)) :: center ! tcx: (kshape_1)
        real(kcircle_1) :: radius = 1.0

        contains

        procedure :: print => printCircle
    end type

    abstract interface
        subroutine printShape (s)
        import shape
            class (shape(4)), intent(in) :: s ! tcx: (4)
        end subroutine
    end interface

    contains

    subroutine printCircle (s)
        class (circle(4,4)), intent(in) :: s ! tcx: (4,4)

        write (*, '(a17,2f15.2)') "circle's center:", s%center%x, s%center%y
        write (*, '(a17,f15.2)') "circle's radius:", s%radius
    end subroutine
end module

module m1
use m
    type plate (kplate_1) ! kplate_1=4
       integer, kind :: kplate_1
        class (shape(kplate_1)), allocatable :: shapes(:) ! tcx: (kplate_1)

        contains

        procedure :: print => printPlate
    end type

    contains

    subroutine printPlate (p)
        class (plate(4)), intent(in) :: p ! tcx: (4)

        if (allocated (p%shapes)) then
            do i = lbound(p%shapes,1), ubound(p%shapes,1)
                call p%shapes(i)%print
            end do
        else
            print *, 'there is no data to print'
        end if
    end subroutine
end module

program abstracti010k
use m1
    type (plate(4)) :: p1, p2 ! tcx: (4)

    allocate (p2%shapes(-1:1), source=(/(circle(4,4)(point(4)(i, 0)), i = -1,1)/)) ! tcx: (4) ! tcx: (4,4)

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


! Extensions to introduce derived type parameters:
! type: point - added parameters (kpoint_1) to invoke with (4) / declare with (4) - 2 changes
! type: shape - added parameters (kshape_1) to invoke with (4) / declare with (4)/(kshape_1) - 2 changes
! type: circle - added parameters (kcircle_1) to invoke with (4,4) / declare with (4,4) - 2 changes
! type: plate - added parameters (kplate_1) to invoke with (4) / declare with (4) - 2 changes
