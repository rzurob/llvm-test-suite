!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/03/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Use of the parent component in the
!                               structure constructor.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len  :: n

        real(k) :: data(n)
        integer(k/2) :: id
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        complex(k) :: cx
        character(l) :: name
    end type

    type(base(8,:)), allocatable :: b1
end module

program dtparamConstr011a
use m
    type (child(8,:,:)), allocatable :: c1

    logical(4), external :: precision_r8, precision_x6

    allocate (b1, source=base(8,53)(data=(/(exp(i/2.1d0), i=1,53)/), id=10))

    allocate (c1, source=child(8,53,20)(base=b1, cx=(1.0d-1, 2.0d-1), &
                name='child type with param: 8,53,20'))

    !! verify
    if ((c1%n /= 53) .or. (c1%l /= 20)) error stop 1_4

    if (c1%name /= 'child type with para') error stop 2_4

    if (.not. precision_x6(c1%cx, (1.0d-1, 2.0d-1))) error stop 3_4

    if (c1%base%id /= 10) error stop 4_4

    do i = 1, 53
        if (.not. precision_r8 (c1%data(i), exp(i/2.1d0))) error stop 5_4
    end do
end
