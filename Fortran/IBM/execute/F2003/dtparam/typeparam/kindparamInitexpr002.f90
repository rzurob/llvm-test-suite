! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/23/2005
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: the default initializations for the
!                               components; use character, logical real type
!                               components with intrinsic functions as the
!                               default init.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k1, k2)
        integer, kind :: k1, k2

        character :: c = char(k1)
        logical :: flag = (k1 < k2)
        real(4) :: data = abs (k1 - k2)
    end type
end module

program kindparamInitexpr002
use m
    type(A(65, 100)), allocatable :: a1

    type (A(77, 20)) a2(10)
    class (A(90, -2)), pointer :: a3(:,:)

    logical(4) precision_r4

    allocate (a1, a3(2, 3))

    !! verify the default initializations for the components
    if ((a1%c /= 'A') .or. (.not. a1%flag) .or. &
        (.not. precision_r4(a1%data, 3.5e1))) error stop 1_4

    if ((a2(1)%c /= 'M') .or. (a2(8)%flag) .or. &
        (.not. precision_r4(a2(6)%data, 5.7e1))) error stop 2_4

    if (any(a3%c /= 'Z') .or. a3(2, 1)%flag .or. &
        (.not. precision_r4(a3(1, 3)%data, 9.2e1))) error stop 3_4
end
