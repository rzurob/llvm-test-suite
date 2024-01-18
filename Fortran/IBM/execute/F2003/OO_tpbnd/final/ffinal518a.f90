!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal518a.f
! %VERIFY: ffinal518a.out:ffinal518a.vf
! %STDIN:
! %STDOUT: ffinal518a.out
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
!*  DESCRIPTION                : final sub (finalization not to be done for
!*                               structure constructor used in DATA statement)
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

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module

module m1
use m, only : base
    type dataType
        type (base) :: b1
    end type

    type dataType2
        type (dataType) :: d1
    end type
end module

program ffinal518a
    call abc
    call abc
end

subroutine abc
use m1
    type (dataType2) :: d1

    data d1%d1%b1 /base(10)/
end subroutine
