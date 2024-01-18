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
! %GROUP: fArg003.f
! %VERIFY: fArg003.out:fArg003.vf
! %STDIN:
! %STDOUT: fArg003.out
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
!*  DATE                       : 04/30/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (the temps created by
!*                               function calls used as actual arg)
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
        integer*4 :: id = 0

        contains

        procedure :: add => produceBase
        procedure :: print => printBase
    end type

    contains

    type (base) function produceBase (b, i)
        class (base), intent(in) :: b
        integer*4, intent(in) :: i

        produceBase%id = b%id + i
    end function

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printVal1 (b)
        class (base), intent(in) :: b

        call b%print
    end subroutine

    subroutine printVal2 (b)
        type (base), intent(in) :: b

        call b%print
    end subroutine
end module

program fArg003
use m
    type (base) :: b1

    b1 = base (1)

    call printVal1 (b1%add (9))

    call printVal2 (b1%add (19))
end
