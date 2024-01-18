!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc511.f
! %VERIFY: falloc511.out:falloc511.vf
! %STDIN:
! %STDOUT: falloc511.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/04/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (abstract type in allocate statement;
!                               a test case derived from fdtio513a1.f)
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
    type, abstract :: base
        contains

        procedure(printBase), deferred :: print
    end type

    interface
        subroutine printBase (b)
        import base
            class (base), intent(in) :: b
        end subroutine
    end interface
end module

module m1
use m
    type, extends(base) :: child
        integer, allocatable :: id

        contains

        procedure :: print => printChild
    end type

    type, extends (child) :: gen3
        character(20) :: name

        contains

        procedure :: print => printGen3
    end type

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        if (allocated (b%id)) then
            write (*, *) b%id
        end if
    end subroutine

    subroutine printGen3 (b)
        class (gen3), intent(in) :: b

        if (allocated (b%id)) then
            write (*, '(i8,2a)') b%id, '; ', b%name
        else
            write (*, *) b%name
        end if
    end subroutine
end module


!program fdtio513a1
program falloc511
use m1
    class (base), allocatable :: b1(:)

    allocate (b1(0:1), source=(/gen3(1, 'xlftest'), gen3(null(), 'team')/))

    call b1(0)%print
    call b1(1)%print
end
