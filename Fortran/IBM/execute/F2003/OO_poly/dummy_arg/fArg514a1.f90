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
! %GROUP: fArg514a1.f
! %VERIFY: fArg514a1.out:fArg514a1.vf
! %STDIN:
! %STDOUT: fArg514a1.out
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
!*  DATE                       : 06/24/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (argument keyword in type
!                               bounds)
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
    type base1
        integer(4) :: id

        contains

        procedure, pass (b1) :: print2 => printB1B2
    end type

    type base2
        character(20) :: name

        contains

        procedure, pass (b2) :: print2 => printB1B2
    end type

    contains

    subroutine printB1B2 (b1, b2)
        class (base1), intent(in) :: b1
        class (base2), intent(in) :: b2

        print *, b1%id, b2%name
    end subroutine
end module

use m
    type (base1) :: b1
    type (base2) :: b2

    b1%id = 10
    b2%name = 'b2'

    call b1%print2 (b2 = b2)

    call b2%print2 (b1 = b1)

    call b1%print2 (b2 = base2('temp'))

    call b2%print2 (b1 = base1 (1))
end
