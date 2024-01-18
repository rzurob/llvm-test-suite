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
!*  DATE                       : 11/28/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statement: An extended type has a scalar,
!                               nonpointer, nonallocatable parent component with
!                               the type and type parameters of the parent type.
!                               Case: try to reference the type parameters
!                               defined by the extended type via parent
!                               component.  Parent has no type parameter.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
    end type

    type, extends(base) :: child(k, l, m)
        integer, kind :: k = 4
        integer, len :: l, m

        real(k) data(l, m)
    end type

    type (child(4, 10, 3)) c1_m
end module

program dtparamExtends011d
use m
    type (child(l=2, m=4)) c1

    print *, c1_m%base%k                    !<-- illegal
    print *, (c1%base%l + c1%base%m)        !<-- illegal
end
