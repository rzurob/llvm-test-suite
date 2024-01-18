!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal500a1.f
! %VERIFY: ffinal500a1.out:ffinal500a1.vf
! %STDIN:
! %STDOUT: ffinal500a1.out
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
!*  DESCRIPTION                :
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

program ffinal500a1
use m
    call abc

    print *, 'end'
end

subroutine abc
use m
    type (base) :: b1 = base (10)
    type (base) :: b2

    print *, 'in abc'
end subroutine
