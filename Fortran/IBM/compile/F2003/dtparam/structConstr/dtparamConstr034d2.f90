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
!*  DATE                       : 03/16/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of arrays with the different ranks
!                               between pointer and target.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        real(k), pointer :: data(:)
    end type
end module

program dtparamConstr034d2
use m
    type (base(8)) b1

    real(4), target :: r1(40)

    real(8), pointer :: d1(:,:)

    allocate (d1(10, 20))

    b1 = base(8) (r1)       !<-- illegal

    b1 = base(8) (d1)       !<-- illegal
end
