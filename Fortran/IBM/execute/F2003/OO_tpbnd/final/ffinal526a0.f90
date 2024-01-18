!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal526a0.f
! %VERIFY: ffinal526a0.out:ffinal526a0.vf
! %STDIN:
! %STDOUT: ffinal526a0.out
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
!*  DESCRIPTION                : final sub (if the entity being finalized is an
!*                               array, each finalizable component of each
!*                               element of that entity is finalized separately)
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

    type dataType
        type (base) :: b1(3)
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffinal526a0

    call abc

    print *, 'end'
end

subroutine abc
use m
    type (dataType) :: d1(5)
end subroutine

