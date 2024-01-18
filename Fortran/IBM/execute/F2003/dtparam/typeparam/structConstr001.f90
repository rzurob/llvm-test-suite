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
!*  DATE                       : 12/23/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscelleneous
!                               Case: A test case eqivalent to
!                               kindparamInitexpr001 without type parameter.
!                               But the compiler is having problems with
!                               structure constructor as the default
!                               initializations for derived type components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A44
        real(4) :: data(2) = 4*4
        integer(4) :: id = 4 + 4
    end type

    type A48
        real(4) :: data(2) = 4*8
        integer(8) :: id = 4 + 8
    end type

    type A84
        real(8) :: data(2) = 8*4
        integer(4) :: id = 8 + 4
    end type

    type A88
        real(8) :: data(2) = 8*8
        integer(8) :: id = 8 + 8
    end type

    type A164
        real(16) :: data(2) = 16*4
        integer(4) :: id = 16 + 4
    end type

    type B48
        type (A48) :: a1 = A48()    !<-- this seems cause trouble
        type (A84) :: a2 = A84 ((/8/4, 8*4/), 8-4)
    end type

    type B44
        type (A44) :: a1 = A44()
        type (A44) :: a2 = A44 ((/4/4, 4*4/), 4-4)
    end type

    type B88
        type (A88) :: a1 = A88()
        type (A88) :: a2 = A88 ((/8/8, 8*8/), 8-8)
    end type
end module

program simulate_kindparamInitexpr001
use m
    logical(4) precision_r4, precision_r8, precision_r6
    external precision_r4, precision_r8, precision_r6

    type (A84) a1
    type (A88) a2(2)
    type (A164) a3

    class (B88), allocatable :: b1(:)
    class (B48), pointer :: b2
    type (B44) b3


    allocate (b1(10), b2)

    !! verify the default initializations
    if ((a1%id /= 12) .or. any(a2%id /= 16) .or. (a3%id /= 20)) error stop 1_4

    if (.not. precision_r8(a1%data(2), 3.2d1)) error stop 2_4
    if (.not. precision_r8(a2(2)%data(1), 6.4d1))   error stop 3_4
    if (.not. precision_r6(a3%data(2), 6.4q1)) error stop 4_4

    if ((b1(6)%a1%id /= 16) .or. (b1(5)%a2%id /= 0)) error stop 5_4
    if ((b2%a1%id /= 12) .or. (b2%a2%id /= 4)) error stop 6_4
    if ((b3%a1%id /= 8) .or. (b3%a2%id /= 0)) error stop 7_4

    if ((.not. precision_r8(b1(3)%a1%data(2), 6.4D1)) .or. &
        (.not. precision_r8(b1(7)%a2%data(1), 1.d0)) .or. &
        (.not. precision_r8(b1(9)%a2%data(2), 6.4d1))) error stop 8_4

    if ((.not. precision_r4(b2%a1%data(1), 3.2e1)) .or. &
        (.not. precision_r8(b2%a2%data(1), 2.0d0)) .or. &
        (.not. precision_r8(b2%a2%data(2),  3.2d1))) error stop 9_4


    if ((.not. precision_r4(b3%a1%data(2), 1.6e1)) .or. &
        (.not. precision_r4(b3%a2%data(1), 1.e0)) .or. &
        (.not. precision_r4(b3%a2%data(2), 1.6e1))) error stop 10_4
end
