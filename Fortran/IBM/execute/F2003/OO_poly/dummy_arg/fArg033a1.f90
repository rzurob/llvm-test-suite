!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg033a1.f
! %VERIFY: fArg033a1.out:fArg033a1.vf
! %STDIN:
! %STDOUT: fArg033a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (actual-arg changed during
!                               the execution of the procedure)
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
        procedure :: assgnID => assignID2Base
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    class (base), pointer :: b1_m

    type (child), target :: c1 = child (1, 'c1')

    contains

    subroutine assignID2Base (b, id)
        class (base), intent(out) :: b
        integer*4, intent(in) :: id

        b%id = b%id
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (b)
        class (base), target, intent(inout) :: b

        b1_m%id = 10

        call b%print
    end subroutine
end module


program fArg033a1
use m
    class (base), pointer :: b1

    allocate (b1, source=child (1, 'b1'))

    b1_m => c1

    call test1 (b1_m)

    call test2 (b1)

    call b1%print

    call b1_m%print

    deallocate (b1)

    contains

    subroutine test2 (b)
        class (base), target, intent(out) :: b

        call b1%assgnID (100)

        call b%print
    end subroutine
end
