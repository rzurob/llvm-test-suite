!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/24/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Structure constructor for parameterized
!                               derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type, extends(base) :: child
        integer(k/2) :: id(n)
    end type

    type, extends(child) :: gen3 (l)
        integer, len :: l

        character(l) :: name
    end type
end module

program dtparamConstr010
use m
    class(base(4,:)), allocatable :: b1
    type (child(8,:)), pointer :: c1(:)
    type (gen3(8, 36, 20)) g1

    logical(4), external :: precision_r4, precision_r8

    allocate (b1, source=base(4,27)((/(i*1.0, i=1,27)/)))

    allocate (child(8, 11) :: c1(10))

    do i = 1, 10
        c1(i) = child(8, 11)(sin((/(i*1.2d0+j*1.2d-1, j=1, 11)/)), &
            (/(i*100+j, j=1, 11)/))
    end do

    g1 = gen3(8, 36, 20)((/(i*1.0d0, i=1, 36)/), (/(i, i=1,36)/),&
        'g1 in the main program')

    !! verify
    if ((b1%n /= 27) .or. (c1%n /= 11)) error stop 1_4

    do i = 1, 27
        if (.not. precision_r4 (b1%data(i), i*1.0)) error stop 2_4
    end do

    do i = 1, 10
        do j = 1, 11
            if (.not. precision_r8(c1(i)%data(j), sin(i*1.2d0+j*1.2d-1))) &
                error stop 3_4

            if (c1(i)%id(j) /= i*100+j) error stop 4_4
        end do
    end do

    print *, g1
end
