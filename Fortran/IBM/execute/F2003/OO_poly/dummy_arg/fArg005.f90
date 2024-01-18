!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg005.f
! %VERIFY: fArg005.out:fArg005.vf
! %STDIN:
! %STDOUT: fArg005.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-allocatable
!*                               dummy-arg scalars)
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
        integer*4 :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name = 'no-name'

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

    subroutine createBase (b, id, name)
        class (base), allocatable, intent(out) :: b
        integer*4, intent(in) :: id
        character(*), optional, intent(in) :: name

        if (present (name)) then
            allocate (b, source=child(id, name))
        else
            allocate (b)
            b%id = id
        end if
    end subroutine
end module

program fArg005
use m
    class (base), allocatable :: b1

    call createBase (b1, 1, 'b1_alloc')

    call b1%print

    call createBase (b1, 2)

    call b1%print
end
