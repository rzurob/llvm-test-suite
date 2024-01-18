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
! %GROUP: fext043.f
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
!*  DATE                       : 11/12/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : EXTENDS (renamed base type in extended type)
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
    type base(k1)
        integer, kind :: k1
        integer(k1) :: id
    end type
end module

module m1
use m, newBase => base
    type, extends (newBase) :: child(n)
        integer, len :: n
        character(n) :: name
    end type
end module

program fext043
use m
use m1
    type (base(4)) :: b1 = base(4)(10)
    type (child(4,20)) :: c1 = child (4,20)(11,'c1')
    type (newBase(4)) :: n1 = base(4)(12)

    b1 = newBase(4)(10)

    b1 = n1

    if (b1%id /= 12) error stop 1_4

    c1%newbase = base (4)(12)

    if (c1%id /= 12) error stop 2_4

    c1%newbase = newBase (4)(15)

    if (c1%id /= 15) error stop 3_4
end
