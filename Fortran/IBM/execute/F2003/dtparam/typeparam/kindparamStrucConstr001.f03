! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/09/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type-parameter in structure
!                               constructor must be specification expressions.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        real(k) :: data (2) = 1.0
    end type

    integer, parameter :: doublePrec = selected_real_kind(14, 306)

    type (base(k=8)) :: b1_m(2) = (/base(k=doublePrec)(3.0), &
                    base(k=8)((/1.0, 2.0/))/)
end module

program kindparamStrucConstr001
use m
    integer, parameter :: singlePrec = selected_real_kind (6,20)

    type (base(singlePrec)) :: b1 = base(singlePrec)((/10.0, 11.0/))

    logical(4), external :: precision_r4, precision_r8

    if ((.not. precision_r8(b1_m(1)%data(1), real(3.0, 8))) .or. &
        (.not. precision_r8(b1_m(1)%data(2), real(3.0, 8)))) error stop 1_4

    if ((.not. precision_r8(b1_m(2)%data(1), real(1.0, 8))) .or. &
        (.not. precision_r8(b1_m(2)%data(2), real(2.0, 8)))) error stop 2_4

    if ((.not. precision_r4(b1%data(1), 10.0)) .or. &
        (.not. precision_r4(b1%data(2), 11.0))) error stop 3_4
end
