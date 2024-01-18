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
! %GROUP: fconstr010a.f
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
!*                               default initialization, leading components
!*                               does NOT need keyword)
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
        integer*4 ::id
        real*4 :: value = 0.0
    end type

    type, extends(base) :: child
        character*20 :: name = ''
    end type

    type (base), save :: b1_m = base (10)
    type (child), save :: c1_m = child (20)
end module

program fconstr010a
use m

    type (base) :: b1
    type (child) :: c1

    b1 = base(1)
    c1 = child(2)

    if ((b1%id /= 1) .or. (b1%value /= 0.0)) error stop 1_4

    if ((c1%id /= 2) .or. (c1%value /= 0.0) .or. (c1%name /= '')) error stop 2_4

    c1 = child (3, 1.0)

    if ((c1%id /= 3) .or. (c1%value /= 1.0) .or. (c1%name /= '')) error stop 3_4

    c1 = child (4, name = 'data c1')

    if ((c1%id /= 4) .or. (c1%value /= 0.0) .or. &
        (c1%name /= 'data c1')) error stop 4_4

    if ((b1_m%id /= 10) .or. (b1_m%value /= 0.0)) error stop 5_4

    if ((c1_m%id /= 20) .or. (c1_m%value /= 0.0) .or. (c1_m%name /= '')) &
                    error stop 6_4
end
