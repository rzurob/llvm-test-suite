!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/27/2005
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type parameter used in the kind type
!                               param for components: used in selected_int_kind
!                               and selected_real_kind.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (range, p, r)
        integer, kind :: range, p, r

        integer(selected_int_kind(range)) :: id(2)
        real (selected_real_kind(p, r)), allocatable :: data(:)
    end type
end module

program kindparamInitexpr006
use m
    type (A(17, 12, 45)) :: a1, a2(10)
    class (A(2, 10, 100)), allocatable :: a3(:)

    logical(4) precision_r8
    external precision_r8

    a1%id = (/2_8**50, 2_8**45/)
    allocate(a1%data(10), source=1.2d39)

    a2(2) = a1
    a2(2)%id = a2(2)%id - 2_8**48
    a2(2)%data = a2(2)%data/8d21

    allocate(a3(10))

    a3(8)%id = (/10, 50/)

    allocate(a3(7)%data(2), source=3.45d89)

    !! verify the results
    if (any(a1%id /= (/1125899906842624_8, 35184372088832_8/))) error stop 1_4

    if ((.not. precision_r8(a1%data(1), 1.2d39)) .or. &
        (.not. precision_r8(a1%data(6), a1%data(1)))) error stop 2_4

    if (any (a2(2)%id /= (/844424930131968_8, -246290604621824_8/))) error stop 3_4

    if ((.not. precision_r8(a2(2)%data(1), 1.5d17)) .or. &
        (.not. precision_r8(a2(2)%data(10), a2(2)%data(1)))) error stop 4_4

    if (any(a3(8)%id  /= (/10, 50/))) error stop 5_4

    do i = 1, 10
        if (i == 7) then
            if ((.not. precision_r8(a3(i)%data(1), 3.45d89)) .or. &
                (.not. precision_r8(a3(i)%data(2), 3.45d89))) error stop 6_4
        else
            if (allocated(a3(i)%data)) error stop 7_4
        end if
    end do
end
