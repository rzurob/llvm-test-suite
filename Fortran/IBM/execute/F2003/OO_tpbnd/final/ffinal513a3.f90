!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal513a3.f
! %VERIFY: ffinal513a3.out:ffinal513a3.vf
! %STDIN:
! %STDOUT: ffinal513a3.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of the actual arg
!*                               associated with INTENT(OUT) dummy-arg)
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
        integer*4 :: id = 0

        contains

        final :: finalizeBase
        final :: finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        implicit type(base) (b)
        dimension b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program ffinal513a3
use m
    interface
        subroutine abc (b, b1)
        use m
            type (base), intent(out) :: b(:)
            type (base), intent(in) :: b1
        end subroutine
    end interface

    type (base), save :: b1(2)

    call abc (b1, base(10))

    if ((b1(1)%id /= 10) .or. (b1(2)%id /= 10)) error stop 1_4
end

subroutine abc (b, b1)
use m
    type (base), intent(out) :: b(:)
    type (base), intent(in) :: b1

    b%id = b1%id
end subroutine
