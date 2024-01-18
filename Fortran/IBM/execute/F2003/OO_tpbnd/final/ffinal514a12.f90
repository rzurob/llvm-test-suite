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
! %GROUP: ffinal514a12.f
! %VERIFY: ffinal514a12.out:ffinal514a12.vf
! %STDIN:
! %STDOUT: ffinal514a12.out
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
!*  DATE                       : 04/19/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : final sub (finalization of temps created by
!*                               function calls during WRITE statement)
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
        integer*4 id

        contains

        final :: finalizeBase
    end type

    interface operator (==)
        logical function baseEqual (b1, b2)
        import base
            class (base), intent(in) :: b1, b2
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base), intent (in) :: b

        print *, 'finalizeBase'
    end subroutine

    type(base) function makeBase (i)
        integer*4, intent(in) :: i

        makeBase%id = i
    end function
end module

program ffinal514a12
use m

    write (*,*) (makeBase(10) == makeBase(2))
end

logical function baseEqual (b1, b2)
use m, only:base
    class (base), intent(in) :: b1, b2

    baseEqual = (b1%id == b2%id)
end function
