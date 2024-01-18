!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass008a.f
! %VERIFY: fclass008a.out:fclass008a.vf
! %STDIN:
! %STDOUT: fclass008a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (poly array entities in array
!                               constructor; use associate construct)
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
    type base
        integer(4) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character (20) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fclass008a
use m
    class (base), allocatable :: b1(:)

    allocate (b1(0:1), source=(/child (1, 'test1'), child (2, 'test2')/))

    associate (x1 => b1, x2 => (/b1/))
        if ((lbound(x1,1) /= 0) .or. (ubound(x1,1) /= 1)) error stop 1_4

        if ((lbound(x2,1) /= 1) .or. (ubound(x2,1) /= 2)) error stop 2_4

        call x1(0)%print
        call x1(1)%print

        call x2(1)%print
        call x2(2)%print
    end associate
end
