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
!*  DATE                       : 09/18/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 325468)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        character(:), allocatable :: str
    end type

    type B
        type(A) a1(2)
    end type
end module

use m
    type(A) s1(2)
    type(B) b1

    s1(1)%str = 'xyz'
    s1(2)%str = 'abcdefg'

    b1 = B(s1)

    if ((.not. allocated(b1%a1(1)%str)) .or. &
        (.not. allocated(b1%a1(2)%str))) error stop 1_4

    if ((b1%a1(1)%str%len /= 3) .or. (b1%a1(2)%str%len /= 7)) &
        error stop 2_4

    if ((b1%a1(1)%str /= 'xyz') .or. (b1%a1(2)%str /= 'abcdefg')) &
        error stop 3_4
end
