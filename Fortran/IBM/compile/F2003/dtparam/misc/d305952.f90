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
!*  DATE                       : 08/20/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 305952)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type A
        integer(1) :: x = -1
    end type

    type (A) a1*8                   !<-- illegal

    print *, a1
    print *, sizeof(a1)

    contains
    
    type (A) function c1*8 (c2)     !<-- illegal
        type(A), intent(in) :: c2*8 !<-- illegal
        c1%x = c2%x
    end function

    end

