!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal515a8.f
! %VERIFY: ffinal515a8.out:ffinal515a8.vf
! %STDIN:
! %STDOUT: ffinal515a8.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (temps created by structure
!*                               constructor finalized; DO-construct)
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

program ffinal515a8
use m
    type (base) :: b1 = base(10)

    do i = 1, b1%diff(base(5))
        print *, i
    end do

    print *, 'end'
end
