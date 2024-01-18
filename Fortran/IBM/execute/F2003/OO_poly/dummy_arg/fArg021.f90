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
! %GROUP: fArg021.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
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
!*  DATE                       : 06/01/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (literals used as the
!                               actual-arg for unlimited poly dummy-arg; also
!                               tests that compiler will use temporaries that
!                               duplicate in value for the calls)
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
    private abc
    contains

    subroutine test1 (y)
        class (*) :: y

        call abc (y)
    end subroutine

    subroutine abc (x)
        class(*), intent(out) :: x
    end subroutine
end module

program fArg021
use m
    call test1 (100_4)
    call test1 (1_2)
    call test1 (2_8)
    call test1 (1.0e2)
    call test1 (2.0d1)
    call test1 (3.0q1)
    call test1 ((1.0,1.0))
    call test1 ((1.0d1,1.0d0))
    call test1 ("test1")
    call test1 (1==2)
end
