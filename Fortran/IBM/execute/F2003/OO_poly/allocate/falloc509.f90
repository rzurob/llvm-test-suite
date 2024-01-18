!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc509.f
! %VERIFY: falloc509.out:falloc509.vf
! %STDIN:
! %STDOUT: falloc509.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (a cross test on select type
!                               construct using the unlimited poly-allocatable)
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
        contains

        procedure, nopass :: print => printBase
    end type

    type, extends(base) :: child
        contains

        procedure, nopass :: print => printChild
    end type

    contains

    subroutine printBase
        print *, 'base'
    end subroutine

    subroutine printChild
        print *, 'child'
    end subroutine
end module

program falloc509
use m
    class (*), allocatable :: x1(:)

    allocate (child:: x1(2))

    i = 1

    do while (i <= 2)
        select type (x => x1(i))
            class is (base)
                call x%print
        end select

        i = i + 1
    end do
end
