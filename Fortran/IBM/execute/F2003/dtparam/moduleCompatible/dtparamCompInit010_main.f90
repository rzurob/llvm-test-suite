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
!*  DATE                       : 04/12/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter (component)
!                               This test case tests the initialization of
!                               component that is of derived type without type
!                               parameter.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamCompInit010
use m
    type (collector(20)) :: co1
    class(collector(300)), allocatable :: co2(:)


    co1 = collector(20)

    allocate (co2(30), source=collector(300))

    !! verify initialization
    do i = 1, 20
        do j = 1, 10
            if (co1%data(i)%id(j) /= 20+j-1) error stop 1_4
        end do
    end do

    do i = 1, 30
        do j = 1, 300
            do k = 1, 10
                if (co2(i)%data(j)%id(k) /= 299+k) error stop 2_4
            end do
        end do
    end do
end
