!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg004a1.f
! %VERIFY: fArg004a1.out:fArg004a1.vf
! %STDIN:
! %STDOUT: fArg004a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-actual-arg
!                               associated with non-poly-dummy-arg; use of the
!                               explicit-shape arrays)
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
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

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


program fArg004a1
use m
    type (child) :: c1 (4)

    c1 = (/(child (i, 'c1'),i=1,4)/)

    call test1 (c1)

    contains

    subroutine test1 (b)
        class (base), intent(in) :: b(4)

        call b(1)%print
        call b(2)%print
        call b(3)%print
        call b(4)%print

        call test2 (b)
    end subroutine

    subroutine test2 (b)
        type(base), intent(in) :: b(4)

        call b(1)%print
        call b(2)%print
        call b(3)%print
        call b(4)%print
    end subroutine
end
