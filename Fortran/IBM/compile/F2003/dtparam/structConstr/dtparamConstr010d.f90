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
!*  DATE                       : 02/24/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: C483, diagnostic test case: Component-spec
!                               used more than once for a component.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        real(k) :: data
    end type

    type, extends(base) :: child(n)
        integer, len :: n

        integer (k) :: id(n)
    end type
end module

program dtparamConstr010d
use m
    type (base(8)) :: b1
    class(base(4)), pointer :: b2

    b1 = base(8)(8, data=3.2d0)         !<-- illegal

    allocate (b2, source=child(4, 20)(1.2, 20, id=(/(j, j=1,20)/))) !<-- illegal
end
