!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/17/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : structure constructor (all components without
!                               default initializations must be supplied in the
!                               structure constructor)
!*
!*  KEYWORD(S)                 :
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
    end type

    type, extends(base) :: child
        character*20 :: name
    end type

end module

program fconstr004d
use m

    type (base) :: b1
    type (child) :: c1, c2

    b1 = base()     !<-- illegal
    c1 = child(1)       !<-- illegal
    c1 = child(1, 'test', 10.0)     !<-- illegal
    c1 = child('test')      !<-- illegal
    c2 = child()        !<-- illegal
end
