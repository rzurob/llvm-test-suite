!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/29/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.4: default init.)
!                               Case: Default initializations for actual-arg
!                               that is associated with intent(out) dummy-arg;
!                               non-polymorphic case.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        real(k) :: data = -1.0
    end type

    type, extends(base) :: child
        integer(k) :: id = -1
    end type

    contains

    subroutine resetBase (b)
        type (base(4)), intent(out) :: b

        logical(4), external :: precision_r4

        if (.not. precision_r4(b%data, -1.0e0)) error stop 10_4
    end subroutine

    subroutine resetChild (c)
        type(child(8)), intent(out) :: c

        logical(4), external :: precision_r8

        if (.not. precision_r8(c%data, real(-1.0e0, 8))) error stop 11_4
        if (c%id /= -1) error stop 12_4
    end subroutine
end module

program dtparamCompInit008
use m
    type (child(8)), allocatable :: b1
    class(base(4)), pointer :: b2(:)
    type (child(4)) c1

    logical(4), external :: precision_r4, precision_r8

    allocate (b1, source=child(8)(1.23d0, 120_8))
    allocate (b2(10), source=child(4)(3.22e0, -100))

    c1%id = 100

    call resetBase(b2(1))
    call resetBase(c1%base)

    call resetChild(b1)

    !! verify again in the main program
    if (.not. precision_r4(c1%data, -1.0)) error stop 1_4
    if (c1%id /= 100) error stop 2_4

    if ((.not. precision_r4 (b2(1)%data, -1.0)) .or. &
        (.not. precision_r4 (b2(3)%data, 3.22e0))) error stop 3_4

    if (.not. precision_r8 (b1%data, real(-1.0, 8))) error stop 4_4
    if (b1%id /= -1) error stop 6_4

    select type (x => b2)
        type is (child(4))
            if ((x(1)%id /= -100) .or. ((x(2)%id /= -100))) error stop 5_4

        class default
            error stop 7_4
    end select
end

