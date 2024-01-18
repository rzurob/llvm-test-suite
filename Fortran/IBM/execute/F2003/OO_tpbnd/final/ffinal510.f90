!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal510.f
! %VERIFY: ffinal510.out:ffinal510.vf
! %STDIN:
! %STDOUT: ffinal510.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (local variables introduced by
!*                               IMPLICIT statement in a subprogram)
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

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
        if (associated (b%data)) deallocate (b%data)
    end subroutine
end module

program ffinal510
use m
    call abc

    print *, 'end of program'
end

subroutine abc
use m
    implicit type (base) (b)

    target b2

    parameter (b_const = base (null()))

    print *, 'end of abc'
end subroutine
