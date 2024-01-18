! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/27/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.4: default init.)
!                               Case: Default initializations for components
!                               that of parameterized derived type with
!                               allocatable component with null().
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k)
        integer, kind :: k

        real(k), allocatable :: r1(:,:)
        complex(k) :: cx
    end type

    type base (k)
        integer, kind :: k = 4

        type(A(k)) :: a1 = A(k)(null(), k)
        type(A(2*k)) :: a11(2) = A(2*k)(null(), 2*k)
    end type
end module

program dtparamCompInit006
use m
    class (base(4)), allocatable :: b1(:)
    type (base(8)) b2
    type (base(8)), pointer ::  b3(:)

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
