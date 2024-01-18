! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/19/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Polymorphic entities with deferred type
!                               parameters in the array constructor; use
!                               type-spec in the array constructor
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n, l)
        integer, len :: n, l

        real :: data(n)
        character(l) :: name
    end type
end module

program dtparamArrConstr003a
use m
    class(base(:,:)), pointer :: b1, b2(:)

    type(base(:,:)), allocatable :: b3(:)

    logical(4), external :: precision_r4

    allocate (b1, source=base(100, 25)(log((/(i*1.0e1, i=1,100)/)), &
            'scalar b1'))

    allocate (b2(2), source=(/base(100,25)((/(i*1.0e1, i=1,100)/), &
            'element 1 of b2'), base(100,25)((/(sin(i*1.0e1), i=1, 100)/), &
            'element 2 of b2')/))


    allocate (b3(0:2), source=(/base(100,25) :: b1, b2/))

    !! verify the values for b3
    if (any(b3%name /= (/character(25) :: 'scalar b1', 'element 1 of b2', &
            'element 2 of b2'/))) error stop 1_4

    do i = 1, 100
        if (.not. precision_r4(b3(0)%data(i), log(i*1.0e1))) error stop 2_4

        if (.not. precision_r4(b3(1)%data(i), i*1.0e1)) error stop 3_4

        if (.not. precision_r4(b3(2)%data(i), sin(i*1.0e1))) error stop 4_4
    end do
end
