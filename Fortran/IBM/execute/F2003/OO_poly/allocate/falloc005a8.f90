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
! %GROUP: falloc005a8.f
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
!*  DATE                       : 07/13/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (substring used in the source-expr)
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
        character(10), allocatable :: address(:)
    end type
end module

program falloc005a8
use m
    type (base) :: b1, b2, b3

    allocate (b1%address(2), source='xlftest 101'(4:))

    if (any (b1%address /= 'test 101')) error stop 1_4

    allocate (b2%address(2:3), source=b1%address(1)(6:8))

    if ((b2%address(2) /= '101') .or. (b2%address(3) /= '101')) error stop 2_4

    allocate (b3%address(0:1), &
            source=(/(b1%address(i)(:5)//char(ichar('0')+i), i=1,2)/))


    if (b3%address(0) /= 'test 1') error stop 3_4
    if (b3%address(1) /= 'test 2') error stop 4_4
end
