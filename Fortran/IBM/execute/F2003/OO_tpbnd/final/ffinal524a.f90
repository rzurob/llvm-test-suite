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
! %GROUP: ffinal524a.f
! %VERIFY: ffinal524a.out:ffinal524a.vf
! %STDIN:
! %STDOUT: ffinal524a.out
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
!*  DATE                       : 04/16/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : final sub (other bindings can be invoked in
!*                               final sub; modify in this case)
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
        integer*4 :: id = 1

        contains

        final :: finalizeBase
        procedure :: print => printBase
        procedure :: add1 => addOne2id
    end type

    contains

    subroutine finalizeBase (b)
        type (base) b

        intent(inout) :: b

        call b%add1
        call b%print
        print *, 'end finalizeBase'
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine addOne2id (b)
        class (base), intent(inout) :: b

        b%id = b%id + 1
    end subroutine
end module

program ffinal524a
    call abc

    call abc
end

subroutine abc
use m
    implicit type(base) (b)

    target b1
    logical, save :: firstTime = .true.

    if (firstTime) then
        b1%id = 10
        firstTime = .false.
    end if
end
