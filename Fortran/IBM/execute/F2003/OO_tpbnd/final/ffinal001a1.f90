!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal001a1.f
! %VERIFY: ffinal001a1.out:ffinal001a1.vf
! %STDIN:
! %STDOUT: ffinal001a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (empty type's finalization)
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
        contains

        FINAL :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'there is noting to do'
    end subroutine
end module

program ffinal001a1
use m

    type (base), pointer :: b1
    type (base), allocatable :: b2

    allocate (b1, b2)

    deallocate (b1, b2)
end
