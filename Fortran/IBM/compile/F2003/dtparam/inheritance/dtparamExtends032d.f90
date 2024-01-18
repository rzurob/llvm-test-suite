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
!                               Case: the type parameter name is the same as the
!                               type name.  The type definition is legal, but
!                               can no longer be extended due to name conflict.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type k (k)          !<-- this type is legal, but it can NOT be extended
        integer, kind :: k = 4

        integer(k) :: id = 100
    end type
end module

program dtparamExtends032d
use m
    type, extends(k) :: l   !<-- illegal
    end type
end
