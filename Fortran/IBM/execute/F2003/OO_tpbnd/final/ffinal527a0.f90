!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal527a0.f
! %VERIFY: ffinal527a0.out:ffinal527a0.vf
! %STDIN:
! %STDOUT: ffinal527a0.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (elemental finalizer)
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

        final :: finalizeBase
        final :: finalizeBaseRank1
    end type

    contains

    elemental subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        b%id = -1
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(inout) :: b(:)

        print *, 'finalizeBaseRank1'
        b%id = -2
    end subroutine
end module

program ffinal527a0
use m
    type (base) :: b1(3)

    b1%id = 0

    call abc (b1)

    contains

    subroutine abc (b)
        class (base), intent(out) :: b(:)

        if (any(b%id /= -2)) error stop 1_4
    end subroutine
end
