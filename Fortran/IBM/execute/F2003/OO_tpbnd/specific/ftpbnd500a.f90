!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd500a.f
! %VERIFY: ftpbnd500a.out:ftpbnd500a.vf
! %STDIN:
! %STDOUT: ftpbnd500a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (C458, a functional test
!                               that external procedure with explicit interface
!                               can be used as type bound; test on NOPASS
!                               binding)
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
    interface
        subroutine printBase
        end subroutine

        integer function countVal (i)
            integer, intent(in) :: i(:)
        end function
    end interface

    type base
        integer id

        contains

        procedure, nopass :: print => printBase
        procedure, nopass :: count => countVal
    end type
end module

subroutine printBase
    print *, 'base'
end subroutine

integer function countVal (i)
    integer, intent(in) :: i(:)

    countVal = sum (i)
end function

program ftpbnd500a
use m
    type (base) b1(10)

    call b1%print

    if (b1%count ((/10, 20/)) /= 30) error stop 1_4
end
