!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal522.f
! %VERIFY: ffinal522.out:ffinal522.vf
! %STDIN:
! %STDOUT: ffinal522.out
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
!*  DESCRIPTION                : final sub (no finalization needed for pointer
!*                               function return)
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

        procedure :: replicate => replicateBase

        final :: finalizeBase
    end type

    contains

    !! NOTE: this is written in a way intened not to get memleak using intrinsic
    !!       assignment
    function replicateBase (b)
        class (base), intent(in) :: b
        type (base), pointer :: replicateBase

        type (base), target, save :: temp

        temp%id = b%id

        replicateBase => temp
    end function

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal522
use m
    type (base), save :: b1, b2

    b1%id = 10

    b2 = b1%replicate()

    print *, b2, b1%replicate()
end
