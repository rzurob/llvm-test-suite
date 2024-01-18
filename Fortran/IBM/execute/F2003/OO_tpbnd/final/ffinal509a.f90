!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal509a.f
! %VERIFY: ffinal509a.out:ffinal509a.vf
! %STDIN:
! %STDOUT: ffinal509a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization happens before default
!*                               initialization)
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
        integer*4 :: id = 10

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent (inout) :: b

        print *, 'reset id to 0'

        b%id = 0
    end subroutine
end module

program ffinal509a
use m
    interface
        subroutine sub (b)
        use m
            class (base), intent(out) :: b
        end subroutine
    end interface

    type (base) :: b1

    b1%id = -1

    print *, 'call sub'

    call sub (b1)

    print *, 'after sub'

    if (b1%id /= 10) error stop 1_4
end

subroutine sub (b)
use m
    class (base), intent (out) :: b

    if (b%id /= 10) error stop 10_4
end subroutine
