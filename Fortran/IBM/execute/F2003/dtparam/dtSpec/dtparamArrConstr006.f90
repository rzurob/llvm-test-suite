!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Derived type components are of unlimited
!                               poly-allocatable; allocation with parameterized
!                               derived-type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type, extends(base) :: child (k2)
        integer, kind :: k2

        integer(max(k, k2)) :: id
    end type
end module

module n
    type container
        class(*), allocatable :: data(:)
    end type
end module

program dtparamArrConstr006
use m
use n
    class(*), allocatable :: x1(:)

    class(base(8,:)), allocatable :: b1(:), b2
    class(base(4,:)), allocatable :: b3(:)

    type(container), allocatable  :: co1(:)

    logical(4), external :: precision_r4, precision_r8

    allocate (co1(10))

    allocate (base(8, 32) :: b1(0:1), b2)
    allocate (child(4,21, 8) :: b3(-1:1))

    b1(0)%data = (/(i*1.0d0, i=1, 32)/)
    b1(1)%data = (/(i-1.0d0, i=1, 32)/)

    b2%data = (/(i*2.0d0-1.0d0, i=1, 32)/)

    allocate (x1(0:2), source=(/base(8,32) :: b1, b2/))

    co1(1) = container(x1)

    deallocate (x1)

    select type (b3)
        type is (child(4,*,8))
            b3(-1)%data = -1.0
            b3(0)%data  = (/(exp(i*1.0), i=1, b3%n)/)
            b3(1)%data  = log((/(i*1.1, i=1, b3%n)/))

            b3%id = (/integer(8):: 1, 2, 3/)

            allocate (x1(0:2), source=(/child(4,21,8) :: b3/))
    end select

    co1(3) = container((/x1/))

    !! verify co1(1) and co1(3)
    select type (x => co1(1)%data)
        type is (base(8,*))
            do i = 1, x%n
                if (.not. precision_r8(x(0)%data(i), i*1.0d0)) error stop 1_4
                if (.not. precision_r8(x(1)%data(i), i-1.0d0)) error stop 2_4
                if (.not. precision_r8(x(2)%data(i), 2.0*i-1.0d0)) error stop 3_4
            end do

        class default
            stop 10
    end select

    select type (x => co1(3)%data)
        class is (child(4,*,8))
            do i = 1, x%n
                if (.not. precision_r4(x(1)%data(i), -1.0)) error stop 4_4
                if (.not. precision_r4(x(2)%data(i), exp(1.0*i))) error stop 5_4
                if (.not. precision_r4(x(3)%data(i), log(i*1.1))) error stop 6_4
            end do

            if (any(x%id /= (/1, 2, 3/))) error stop 7_4

        class default
            stop 20
    end select
end
