!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg005a10.f
! %VERIFY: fArg005a10.out:fArg005a10.vf
! %STDIN:
! %STDOUT: fArg005a10.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-pointer dummy-arg
!*                               together with nonpointer-poly-dummy-arg; also
!*                               put the call as the type-bound)
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
        procedure, non_overridable :: copyData => copyBaseData
        final :: finalizeBase
    end type

    type, extends (base) :: child
        character*20 :: name = 'no-name'

        contains

        procedure :: print => printChild

        final :: finalizeChild
    end type

    contains

    subroutine copyBaseData (b, b1)
        class (base), intent(in) :: b
        class (base), intent(out), pointer :: b1

        allocate (b1, source=b)
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program fArg005a10
use m
    type (base) :: b1
    type (child) :: c1

    class (base), pointer :: b2, b3

    print *, 'begin'

    b1%id = 10
    c1%id = 20
    c1%name = 'c1'

    call b1%copyData (b2)

    call b2%print

    call c1%copyData (b3)

    call b3%print

    deallocate (b2)
    deallocate (b3)

    print *, 'end'
end
