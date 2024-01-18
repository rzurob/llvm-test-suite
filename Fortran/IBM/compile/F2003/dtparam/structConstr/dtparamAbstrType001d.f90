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
!*  DATE                       : 02/22/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: C482
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: base (k, n)
        integer, kind :: k = 8
        integer, len :: n

        real(k) :: data(n)
    end type

    type, extends(base) :: child

    end type

    class(base(8,:)), allocatable :: b1
end module

program dtparamAbstrType001d
use m
    allocate (b1, source=base(8,100)(1.0d0))    !<-- illegal

    b1 = base(8, 200)((/(i*1.0d0, i=1, 200)/))  !<-- illegal
end
