!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc014.f
! %VERIFY: falloc014.out:falloc014.vf
! %STDIN:
! %STDOUT: falloc014.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 08/27/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE ((de)allocate statement will be implicitly
!                               invoked during the intrinsic assignment)
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

        contains

        procedure, pass (p) :: print => printPoint
    end type

    type, extends(point) :: point3D
        real(4) :: z

        contains

        procedure, pass (p) :: print => printPoint3D
    end type

    contains

    subroutine printPoint (p)
        class (point), intent(in) :: p

        write (*, '(2f15.2)') p%x, p%y
    end subroutine

    subroutine printPoint3D (p)
        class (point3D), intent(in) :: p

        write (*, '(3f15.2)') p%x, p%y, p%z
    end subroutine
end module


module m1
use m
    type dataType
        class (point), allocatable :: data
    end type
end module

program falloc014
use m1
    type (dataType) :: d1, d2
    class (point), allocatable :: p1

    allocate (d2%data, source=point3D (1.0, 2.5, z=-1.3))

    !! intrinsic assignment causes allocattion of d1%data
    d1 = d2

    call d1%data%print

    allocate (p1)

    p1%x = -10.3
    p1%y = 2.3

    !! intrinsic assignment causes deallocation and reallocation of d2%data
    d2 = dataType (p1)

    call d2%data%print

    deallocate (d2%data)

    !! deallocate will be implicitly invoked by this assignment
    d1 = d2

    if (allocated (d1%data) .or. allocated (d2%data)) error stop 1_4
end
