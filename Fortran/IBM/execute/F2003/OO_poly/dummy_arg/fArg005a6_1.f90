!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg005a6_1.f
! %VERIFY: fArg005a6_1.out:fArg005a6_1.vf
! %STDIN:
! %STDOUT: fArg005a6_1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited poly-pointer
!                               dummy-arg to be associated only with unlimited
!                               poly-pointer actual-arg)
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
    contains

    subroutine createObj (x, x1)
        class (*), pointer, intent(out) :: x
        class (*), intent(in) :: x1

        allocate (x, source=x1)
    end subroutine
end module

module m1
    type base
        integer(4) id

        contains

        final :: finalizeBase
    end type

    type, extends (base) :: child
        character(20) :: name

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program fArg005a6_1
use m
use m1
    class (*), pointer :: x => null()
    type (child) :: c1

    call createObj (x, base(10))

    if (.not. associated (x)) error stop 1_4

    print *, 'deallocating x'

    deallocate (x)

    call createObj (x, c1)

    deallocate (x)

    print *, 'end'
end
