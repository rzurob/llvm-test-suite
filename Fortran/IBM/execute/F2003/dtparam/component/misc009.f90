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
!*  DATE                       : 01/27/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 315570)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A4! (k)
        real(4), allocatable :: r1(:,:)
        complex(4) :: cx
    end type

    type A8! (k)
        real(8), allocatable :: r1(:,:)
        complex(8) :: cx
    end type

    type A16! (k)
        real(16), allocatable :: r1(:,:)
        complex(16) :: cx
    end type

    type base4! (k)
        type(A4) :: a1 = A4(null(), 4)
        type(A8) :: a11(2) = A8(null(), 2*4)
    end type

    type base8! (k)
        type(A8) :: a1 = A8(null(), 8)
        type(A16) :: a11(2) = A16(null(), 2*8)
    end type
end module

program dtparamCompInit006
use m
    class (base4), allocatable :: b1(:)
    type (base8) b2
    type (base8), pointer ::  b3(:)

    logical(4), external :: precision_x8, precision_x6, precision_x3

    allocate (b1(10), b3(30))


    !! verify for the default initializations
    do i = 1, 10
        if (allocated(b1(i)%a1%r1) .or. allocated(b1(i)%a11(1)%r1) .or. &
            allocated(b1(i)%a11(2)%r1)) error stop 1_4

        if ((.not. precision_x8(b1(i)%a1%cx, cmplx(real(4, 4)))) .or. &
            (.not. precision_x6(b1(i)%a11(mod(i,2)+1)%cx, cmplx(real(8, 8), &
            kind=8)))) error stop 2_4
    end do

    if (allocated(b2%a1%r1) .or. allocated(b2%a11(1)%r1) .or. &
        allocated(b2%a11(2)%r1)) error stop 3_4

    if ((.not. precision_x6(b2%a1%cx, cmplx(real(8,8), kind=8))) .or. &
        (.not. precision_x3(b2%a11(1)%cx, cmplx(real(16,16), kind=16))) .or. &
        (.not. precision_x3(b2%a11(2)%cx, cmplx(real(16,16), kind=16)))) error stop 4_4

    do i = 1, 30
        if (allocated(b3(i)%a1%r1) .or. allocated(b3(i)%a11(1)%r1) .or. &
            allocated(b3(i)%a11(2)%r1)) error stop 5_4

        if ((.not. precision_x6(b3(i)%a1%cx, cmplx(real(8,8), kind=8))) .or. &
            (.not. precision_x3(b3(i)%a11(1)%cx, cmplx(real(16,16),kind=16))) .or. &
            (.not. precision_x3(b3(i)%a11(2)%cx, cmplx(real(16,16),kind=16)))) &
                    error stop 6_4
    end do
end
