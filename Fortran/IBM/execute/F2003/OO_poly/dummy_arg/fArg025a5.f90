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
! %GROUP: fArg025a5.f
! %VERIFY: fArg025a5.out:fArg025a5.vf
! %STDIN:
! %STDOUT: fArg025a5.out
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
!*  DATE                       : 06/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (specific-name vs generic
!                               name in actual argument)
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
    contains

    subroutine runTest (sub, i)
        procedure () sub
        integer*4, optional :: i

        if (present (i)) then
            call sub (i)
        else
            call sub
        end if
    end subroutine
end module

program fArg025a5
use m

    interface test1
        subroutine test1 (i)
            integer*4 :: i
        end subroutine

        subroutine test2 (r)
            real*4 :: r
        end subroutine

        subroutine test3
        end subroutine
    end interface

    call runTest (test1, 10)

    call runTest (test3)
end

subroutine test1 (i)
    integer*4 :: i

    print *, 'test1'
end subroutine

subroutine test2 (r)
    real*4 :: r

    print *, 'test2'
end subroutine

subroutine test3

    print *, 'test3'
end subroutine
