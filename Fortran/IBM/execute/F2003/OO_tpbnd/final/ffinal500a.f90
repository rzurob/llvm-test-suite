!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal500a.f
! %VERIFY: ffinal500a.out:ffinal500a.vf
! %STDIN:
! %STDOUT: ffinal500a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/31/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final (static variables not finalized;
!variables in subprogram)
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

    subroutine finalizeBase(b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal500a
use m
    call abc
end

subroutine abc
use m
    type (base), save :: b1
    type (base), static :: b2

    print *, 'in abc'

    b1%id = 10
    b2%id = 20
end subroutine
