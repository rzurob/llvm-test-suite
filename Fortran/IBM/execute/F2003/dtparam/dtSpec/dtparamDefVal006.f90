! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/13/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Structure constructor using default type
!                               patameter values.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k = 8
        integer, len :: n = 100

        real(k) :: data(n)
    end type

    type(base), parameter :: b_const = base((/(i*1.0d0, i=1,100)/))

    type, extends(base) :: child (l)
        integer, len :: l = 20

        character(l) :: name
        integer(k) :: ids(n)
    end type

    type (child), parameter :: c_const(10) = child(1.0d10, 'xlftest', 1)
end module

program dtparamDefVal006
use m
    class(base), allocatable :: b1(:)
    type (child), pointer :: b2
    logical(4), external :: precision_r8

    allocate (b1(10), source=(/(base((/(i*1.0d2+j, j=0,99)/)), i=1, 10)/))
    allocate (b2, source=child((/(i*1.0d0, i=1, 100)/), 'xlftets b2', &
            (/(i*10, i=0, 99)/)))

    !! verify
    do i = 1, 10
        do j = 1, 100
            if (.not. precision_r8(b1(i)%data(j), i*1.0d2+j-1)) error stop 1_4

            if (.not. precision_r8(c_const(i)%data(j), 1.0d10)) error stop 2_4
            if (c_const(i)%ids(j) /= 1) error stop 4_4
        end do

        if (c_const(i)%name /= 'xlftest') error stop 3_4
    end do

    do i = 1, 100
        if (.not. precision_r8 (b_const%data(i), 1.0d0*i)) error stop 5_4

        if (.not. precision_r8 (b2%data(i), i*1.0d0)) error stop 6_4
        if (b2%ids(i) /= (i-1)*10) error stop 7_4
    end do

    if (b2%name /= 'xlftets b2') error stop 8_4
end
