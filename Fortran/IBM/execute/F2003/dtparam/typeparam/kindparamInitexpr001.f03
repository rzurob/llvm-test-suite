! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/23/2005
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Stmt: A kind type parameter may be used in
!                               initialization and specification expressions
!                               within the derived-type definition (4.5.1) for
!                               the type.
!
!                               Case: kind type parameter used for components'
!                               default initializations.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k1, k2)
        integer, kind :: k1, k2

        real(k1) :: data(2) = k1*k2
        integer(k2) :: id = k1 + k2
    end type

    type B (k1, k2)
        integer, kind :: k1, k2

        type (A(k1, k2)) :: a1 = A(k1,k2)()
        type (A(k2, k1)) :: a2 = A(k2, k1) ((/k2/k1, k2*k1/), k2-k1)
    end type
end module

program kindparamInitexpr001
use m
    logical(4) precision_r4, precision_r8, precision_r6
    external precision_r4, precision_r8, precision_r6

    type (A(8, 4)) a1
    type (A(8, 8)) a2(2)
    type (A(16, 4)) a3

    class (B(8, 8)), allocatable :: b1(:)
    class (B(4, 8)), pointer :: b2
    type (B(4,4)) b3


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
