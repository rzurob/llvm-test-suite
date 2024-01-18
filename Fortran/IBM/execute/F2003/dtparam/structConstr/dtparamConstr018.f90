! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Structure constructor with component-spec
!                               is not available if it has inaccessible
!                               components; Solution 1: component with default
!                               initialization.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        private

        real(k) :: data(n) = -1.0
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        private

        integer(k) :: id = -1
        character(l) :: name = 'default'
    end type

    type(base(4, 10)), save :: b1

    logical(4), external :: precision_r4, precision_r8

    contains

    subroutine verifyBase4 (b)
        type(base(4, 10)), intent(in) :: b

        do i = 1, 10
            if (.not. precision_r4(b%data(i), -1.0)) error stop 1_4
        end do
    end subroutine

    subroutine verifyChild8 (c)
        class (child(8, 433, 5)), intent(in) :: c

        do i = 1, 433
            if (.not. precision_r8(c%data(i), -1.0d0)) error stop 2_4
        end do

        if (c%name /= 'defau') error stop 3_4
        if (c%id /= -1) error stop 4_4
    end subroutine
end module

program dtparamConstr018
use m
    type (child(8, 433, 5)) :: c1 = child(8, 433, 5)()

    b1 = base(4, 10)()


    !! verify
    call verifyBase4 (b1)

    call verifyChild8 (c1)
end
