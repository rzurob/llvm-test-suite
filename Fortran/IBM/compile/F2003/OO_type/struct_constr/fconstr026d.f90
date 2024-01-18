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
!*                                                                     
!*  TEST CASE TITLE            :
!*                                                                     
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/17/2005
!*  ORIGIN                     :
!*                             :
!*                                                                     
!*  DESCRIPTION                : structure constructor (unnamed objects from
!*                               structure constructor can not be used to
!*                               initialize the pointer components)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        type (x), pointer :: x1 => null()
    end type

    type x
    end type
end module

program fconstr026d
use m

    type (base) :: b1
    b1 = base (x())     !<-- target attribute is required
end

