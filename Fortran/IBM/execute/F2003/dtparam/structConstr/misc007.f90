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
!*  DATE                       : 03/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 317044)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program misc007
    type base8_20! (k, n)
        real(8) :: data(20)
        integer(1) :: kindVal
    end type

    type container8_20! (k,n)
        type (base8_20) :: data = base8_20(data=(/(-1.0*i, i=1,20)/), kindVal=8)
    end type

    logical(4), external :: precision_r8

    type(container8_20) co1

    co1 = container8_20()

    do i = 1, 20
        if (.not. precision_r8(co1%data%data(i), -1.0d0*i)) error stop 1_4
    end do

    if (co1%data%kindVal /= 8) error stop 2_4
end
