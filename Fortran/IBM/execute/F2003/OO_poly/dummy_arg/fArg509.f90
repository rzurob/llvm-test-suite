!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg509.f
! %VERIFY: fArg509.out:fArg509.vf
! %STDIN:
! %STDOUT: fArg509.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : dummy-arg (intent(out) attribute: finalization
!*                               and default initialization together)
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
        integer*4, pointer :: data => null()
        integer*4 :: id = 0

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        b%id = -1

        if (associated (b%data)) then
            print *, 'deallocating data'
            deallocate (b%data)
        end if
    end subroutine
end module

program fArg509
use m
    type (base) b1

    allocate (b1%data)

    b1%id = 100

    call abc (b1)

    if (associated (b1%data)) error stop 2_4

    contains

    subroutine abc (a)
        type (base), intent(out) :: a

        if (a%id /= 0) error stop 1_4
    end subroutine
end
