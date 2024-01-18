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
! %GROUP: fconstr010.f
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
!*  DATE                       : Nov. 12, 2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (NOT all components have
!*                               default initialization, omitting those have)
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
        integer*4 ::id = 1
        real*4 :: value
    end type

    type, extends(base) :: child
        character*20 :: name = ''
    end type

    type(base), save :: b1_m
    type(child), save :: c1_m

    contains

    subroutine initializeB1_m
        b1_m = base (value = 10.0)
    end subroutine

    subroutine initializeC1_m
        c1_m = child (value = 15.0, name ='module data c1_m')
    end subroutine

end module

program fconstr010
use m

    type (base) :: b1
    type (child) :: c1

    call initializeB1_m
    call initializeC1_m

    b1 = base(value = 1.0)
    c1 = child(value = 2.0)

    if ((b1_m%id /= 1) .or. (b1_m%value /= 10.0)) error stop 1_4

    if ((c1_m%id /= 1) .or. (c1_m%value /= 15.0) .or. &
        (c1_m%name /= 'module data c1_m')) error stop 2_4

    if ((b1%id /= 1) .or. (b1%value /= 1.0)) error stop 3_4

    if ((c1%id /=1) .or. (c1%value /= 2.0) .or. (c1%name /= '')) error stop 4_4

end
