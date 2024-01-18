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
! %GROUP: fArg514a.f
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
!*  DESCRIPTION                : argument association (argument keyword used in
!                               binding calls)
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

        procedure, pass (b) :: sum => addAll
    end type

    contains

    integer*4 function addAll (a, b, c)
        real*4, intent(in) :: a
        class(base), intent(in) :: b
        integer*4, intent(in) :: c

        if (a < 1.0) then
            addAll = (1+c+b%id)
        else
            addAll = (a + c + b%id)
        end if
    end function
end module

program fArg514a
use m
    type (base) :: b1 = base (10)

    if (addAll (-1.5, b1, 5) /= 16) error stop 1_4

    if (b1%sum (3.4, 5) /= 18) error stop 2_4

    if (b1%sum (0.6, c = 5) /= 16) error stop 3_4

    if (b1%sum (c = 5, a = 1.7) /= 16) error stop 4_4

    if (b1%sum (a = 15.1, c = 5) /= 30) error stop 5_4
end
