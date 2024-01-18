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
!*  DATE                       : 11/24/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statement: An extended type includes all of the
!                               type parameters if its parent. Type parameter is
!                               a class (2) local entity (which can not have
!                               more than one identifiers with the same name).
!                               Case: extended type declares a type parameter
!                               uses the a name that is inherited from its
!                               parent)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k = 4
    end type

    type, extends(base) :: child (k)    !<-- illegal
        integer, len :: k = 10
    end type
end module

program dtparamExtends009d
end
