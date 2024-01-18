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
!*  DATE                       : 11/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 313984)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    implicit real(8) (n)

    logical(4), external :: precision_r8
    integer x(5)

    associate(i => x )
        i = (/ ( j, j = 1, 5 ) /)
        k = 6

        n1 = -10
    end associate

    if (k /= 6) error stop 1_4

    if (.not. precision_r8 (n1, -1.0d1)) error stop 2_4

end

