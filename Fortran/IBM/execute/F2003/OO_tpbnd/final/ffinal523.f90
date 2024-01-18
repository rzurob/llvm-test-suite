!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal523.f
! %VERIFY: ffinal523.out:ffinal523.vf
! %STDIN:
! %STDOUT: ffinal523.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of the temporaries in
!*                               final subroutine -- ouch)
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
    end type

    contains

    recursive subroutine finalizeBase (b)
        type (base), intent(in) :: b

        integer*4, save :: counts = 0

        if (counts < 5) then
            counts = counts + 1
            print *, 'finalizeBase', base(10)
        else
            return
        end if
    end subroutine
end module

program ffinal523
use m
    call abc
end

subroutine abc
use m
    type (base) :: b1
end subroutine
