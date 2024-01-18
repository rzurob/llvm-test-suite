! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specific/ftpbnd500a.f
! opt variations: -ql

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 12/09/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
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

    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

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
    type (base(4)) b1(10)

    call b1%print

    if (b1%count ((/10, 20/)) /= 30) error stop 1_4
end
