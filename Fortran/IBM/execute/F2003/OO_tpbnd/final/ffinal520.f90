!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal520.f
! %VERIFY: ffinal520.out:ffinal520.vf
! %STDIN:
! %STDOUT: ffinal520.out
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
!*  DESCRIPTION                : final sub (derived type with component having
!*                               default initialization does NOT imply SAVE
!*                               attribute)
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
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal520
    call abc
    call abc
end

subroutine abc
use m
    type (base) :: b1 = base (10)
    type (base) :: b2

    if (b2%id /= 0) error stop 1_4

    print *, 'end of abc'
end subroutine
