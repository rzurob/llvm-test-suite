!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal515a10.f
! %VERIFY: ffinal515a10.out:ffinal515a10.vf
! %STDIN:
! %STDOUT: ffinal515a10.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (temp created by structure
!*                               constructor in IF-construct get finalized)
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
        procedure, pass(b1) :: diff => IDdiff
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    integer*4 function IDdiff (b1, b2)
        type (base), intent(in) :: b2
        class (base), intent(in) :: b1

        IDdiff = (b1%id - b2%id)
    end function
end module

program ffinal515a10
use m
    type (base) :: b1 = base(10)

    if (b1%diff (base(6)) < 10) then
        print *, 'test 1'
    else
        print *, 'error'
    end if

    if (b1%diff (base(100)) >0) then
        print *, 'error'
    else
        print *, 'test 2'
    end if

    print *, 'end'
end
