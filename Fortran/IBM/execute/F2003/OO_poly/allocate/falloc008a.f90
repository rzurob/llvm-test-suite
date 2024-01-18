!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc008a.f
! %VERIFY: falloc008a.out:falloc008a.vf
! %STDIN:
! %STDOUT: falloc008a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (subsequent redefinition of lower and
!                               upper bounds values should not affect the array
!                               allocation)
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
        character(15) :: name

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        integer(4) :: id

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%name, b%id
    end subroutine

    integer(4) function allocateBasePtr (b, lb, ub, source)
        class (base), pointer, intent(out) :: b(:)
        class (base), intent(in) :: source
        integer(4), intent(inout) :: lb, ub

        integer(4) error

        error = 1

        allocate (b(lb:ub), source=source, stat=error)

        lb = 1
        ub = 0
        allocateBasePtr = error
    end function
end module

program falloc008a
use m
    class (base), pointer :: b1(:)
    integer(4) :: lb, ub

    lb = 0
    ub = 2

    if (allocateBasePtr (b1, lb, ub, child('array_of_3', id = 3)) /= 0)  &
            error stop 1_4


    call b1(0)%print
    call b1(1)%print
    call b1(2)%print
end
