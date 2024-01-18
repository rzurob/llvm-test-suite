!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/30/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.4: default init.)
!                               Case: Default initializations for actual-arg
!                               that is associated with intent(out) dummy-arg;
!                               poly dummy-arg.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        real(k) :: data = -1.0
    end type

    type, extends(base) :: child (n)
        integer, len :: n

        class (base(k)), pointer :: p1 => null()
        integer(k) :: ids(n) = -1
    end type

    logical(4), external :: precision_r4, precision_r8

    contains

    subroutine resetBase4(b)
        class (base(4)), intent(out) :: b

        if (.not. precision_r4(b%data, -1.0)) error stop 10_4

        select type (b)
            type is (base(4))
            type is (child(4,*))
                if (associated(b%p1)) error stop 11_4

                do i = 1, b%n
                    if (b%ids(i) /= -1) error stop 12_4
                end do

            class default
                error stop 13_4
        end select
    end subroutine

    subroutine resetBase8 (b)
        class (base(8)), intent(out) :: b(:)

        do i = 1, size(b)
            if (.not. precision_r8(b(i)%data, real(-1.0, 8))) error stop 15_4

            select type (x => b(i))
                type is (base(8))
                type is (child(8, *))
                    if (associated (x%p1)) error stop 16_4

                    if (any (x%ids /= (/(-1, j=1, x%n)/))) error stop 17_4
                class default
                    error stop 20_4
            end select
        end do
    end subroutine
end module

program dtparamCompInit009
use m
    class (base(4)), allocatable :: b4_1(:)
    class (base(8)), pointer :: b8_1(:)

    type (child(4, 10)) :: c1(100)

    allocate (b4_1(20), source=(/(base(4)(i*1.0), i = 1, 20)/))
    allocate (b8_1(30), source=(/(child(8,15)(i*1.2d1, null(), 100), &
            i=1, 30)/))

    c1%data = (/(i*1.1e0, i=1,100)/)

    do i = 1, 10
        c1%ids(i) = i
    end do

    call resetBase4(b4_1(8))
    call resetBase4(c1(2))

    call resetBase8 (b8_1)

    !! verify
    do i = 1, 20
        if (i /= 8) then
            if (.not. precision_r4(b4_1(i)%data, i*1.0)) error stop 1_4
        else
            if (.not. precision_r4(b4_1(i)%data,-1.0)) error stop 2_4
        end if
    end do

    if ((.not. precision_r4(c1(3)%data, 3.3e0)) .or. &
        (any (c1(1)%ids /= (/(j, j=1, 10)/)))) error stop 3_4

    if (.not. precision_r4(c1(2)%data, -1.0)) error stop 4_4

    if (any (c1(2)%ids /= -1)) error stop 5_4

    do i = 1, 30
        if (.not. precision_r8(b8_1(i)%data, real(-1.0, 8))) error stop 6_4

        select type (x => b8_1(i))
            type is (child(8, *))
                if (any(x%ids /= -1)) error stop 7_4

            class default
        end select
    end do
end
