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
! %GROUP: fconstr501.f
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
!*  DATE                       : 06/18/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : structure constructor (self data in structure
!                               constructor)
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
        integer*4 :: id = -1
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'
    end type
end module

program fconstr501
use m
    type (child) :: c1, c2 (3)

    c1 = child (1, 'c1')

    c2 = (/child(10, 'c1_1'), child(20, 'c1_2'), child(30, 'c1_3')/)

    c1 = child (base = c1%base, name = 'c1')

    if ((c1%id /= 1) .or. (c1%name /= 'c1')) error stop 1_4

    c2 (1:2) = (/(child (c2(i)%id, c2(i)%name), i=2,3)/)

    if (any (c2%id /= (/20, 30, 30/))) error stop 2_4

    if (any (c2%name /= (/'c1_2', 'c1_3', 'c1_3'/))) error stop 3_4
end
