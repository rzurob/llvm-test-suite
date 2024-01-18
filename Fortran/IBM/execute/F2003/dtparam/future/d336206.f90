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
!*  DATE                       : 01/19/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Kind type parameter used in the
!                               initialization expression: HUGE(), DIGITS().
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type numberModel (k1, k2)
        integer, kind :: k1
        integer(k1), kind :: k2 = 0

        integer(k1) :: maxVal = huge(k2)
        integer(k1) :: digitVal = digits(k2)
    end type
end module

program kindparamComp005
use m
    type (numberModel(4)) nm4(2)
    type (numberModel(8)) nm8
    type (numberModel(2)) nm2(100)
    type (numberModel(1, 19)) nm1

    if ((nm4(2)%maxVal /= huge(1_4)) .or. (nm4(1)%digitVal /= digits(10_4))) &
            error stop 1_4

    if ((nm8%maxVal /= huge(2_8)) .or. (nm8%digitVal /= digits(-1_8))) &
            error stop 2_4

    if ((nm2(100)%maxVal /= huge(100_2)) .or. &
        (nm2(10)%digitVal /= digits(-10_2))) error stop 3_4

    if ((nm1%maxVal /= huge(1_1)) .or. (nm1%digitVal /= digits(-10_1))) &
            error stop 4_4
end
