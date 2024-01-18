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
!*  DATE                       : 11/22/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtParam (section 4.5.6.1: inheritance.  Stmt:
!                               An extended type includes all of the type
!                               parameters of its parent type.
!                               Case: use parent's dt-parameters in the
!                               extended type definition.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, l)
        integer, kind :: k
        integer(8), len :: l
        private                 !<-- this has no effects
    end type
end module

module m1
use m
    type, extends(base) :: child
        integer(k) :: id
        real(k) :: data(l) = 1.0
    end type

    type (child(4, 10)), save :: c1_m
end module

program dtparamExtends002
use m1
    type (child(8, l=110)) c1

    logical(4) precision_r4, precision_r8

    !! verify the type parameters for c1_m and c1
    if ((c1_m%k /= 4) .or. (c1_m%l /= 10)) error stop 1_4
    if (size(c1_m%data) /= 10) error stop 2_4

    if ((c1%k /= 8) .or. (c1%l /= 110)) error stop 3_4
    if (size(c1%data) /= 110) error stop 4_4

    !! verify that c1_m%data and c1%data are all initialized to be zero
    do i = 1, size(c1_m%data)
        if (.not. precision_r4 (c1_m%data(i), 1.0_4)) error stop 5_4
    end do


    do i = 1, size(c1%data)
        if (.not. precision_r8 (c1%data(i), 1.0_8)) error stop 6_4
    end do
end
