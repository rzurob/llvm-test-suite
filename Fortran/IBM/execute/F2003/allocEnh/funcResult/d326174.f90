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
!*  DATE                       : 10/03/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 326174)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(*), parameter :: c_const(0:1, 0:1) = &
        reshape([character(3):: '00', '10', '01', '11'], [2,2])

    character(:), allocatable :: c(:,:)

    c = c_const

    if (any(lbound(c) /= 0) .or. any(ubound(c) /= 1)) error stop 1_4

    ii = c_const%len
    if (ii /= 3) error stop 2_4


    ii = c_const(1,1)%len
    if (ii /= 3) error stop 3_4


    ii = c%len
    if (ii /= 3) error stop 4_4

    ii = c(1,1)%len
    if (ii /= 3) error stop 5_4

    end
