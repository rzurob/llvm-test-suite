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
!*  DATE                       : June 8, 2009
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : Miscellansous (defect 353759)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type X (k)
        integer, kind :: k

        real(k) :: data
    end type
end module

use m
    type(X(4)) x1(1)

    print *, [X :: x(1)] !<-- X as type-spec in AC without parameter value

    x1 = [X :: x(4)(1.0)]   !<-- X as a type-spec is wrong without type-parameter value
end

