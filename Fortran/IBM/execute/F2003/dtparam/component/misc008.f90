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
!*  DATE                       : 01/26/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 315531)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base10! (n)
!        integer, len :: n

        integer :: id (10)
    end type

    type collector
        type(base10) :: b1(10) = (/(base10((/(i, i=j,j+9)/)), j=1,10)/)
    end type
end module

program dtparamCompInit001
use m
    type (collector) co1
    class(collector), allocatable :: co2

    allocate(co2)

    do i = 1, 10
        if (any (co1%b1(i)%id /= (/(j, j=i, i+9)/))) error stop 1_4
        if (any (co2%b1(i)%id /= (/(j, j=i, i+9)/))) error stop 2_4
    end do
end
