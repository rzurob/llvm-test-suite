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
!*  DATE                       : 12/31/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type parameters used as the length
!                               type parameters for the derived type components
!                               and bounds of array components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type array (lbound, ubound)
        integer, len :: lbound, ubound

        integer data(lbound:ubound)
    end type

    type base(k1, k2)
        integer, kind :: k1 = 1, k2 = 10

        real :: data(k1-1:k2+1)
        type(array(k1, k2)) :: a1
    end type
end module

program kindparamSpecexpr003
use m
    logical(4), external :: precision_r4
    type(base) b1(10)
!    type(base(10, 1)) b2
    type(base(10, 1000)) b3

    b1(2)%data = (/(i*1.0e0, i=0, 11)/)
    b1(1)%a1%data = (/(i, i=1,10)/)

    b3%data = (/(i*1.0e0, i=1001, 9, -1)/)
    b3%a1%data= -1

    !! verify the results
    if (any(b1(1)%a1%data /= (/1,2,3,4,5,6,7,8,9,10/))) error stop 1_4

    do i = 0, 11
        if (.not. precision_r4(b1(2)%data(i), i*1.0e0)) error stop 2_4
    end do

!    if ((size(b2%data) /= 0) .or. (size(b2%a1%data) /= 0)) error stop 3_4

    do i = 10, 1000
        if (b3%a1%data(i) /= -1) error stop 4_4

        if (.not. precision_r4(b3%data(i), (1010-i)*1.0e0)) error stop 5_4
    end do

    if ((.not. precision_r4(b3%data(9), 1001*1.0e0)) .or. &
        (.not. precision_r4(b3%data(1001), 9*1.0e0))) error stop 6_4
end
