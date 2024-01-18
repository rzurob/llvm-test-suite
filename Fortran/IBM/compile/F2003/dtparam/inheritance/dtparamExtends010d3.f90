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
!*  DATE                       : 12/20/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: type parameter name conflict with binding
!                               name; use generic name.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        contains

        procedure :: print => printBase
        generic :: p => print
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b
    end subroutine
end module

program dtparamExtends010d3
use m
    type, extends(base) :: child (p)    !<-- illegal
        integer, kind :: p

        integer(p) :: val
    end type
end
