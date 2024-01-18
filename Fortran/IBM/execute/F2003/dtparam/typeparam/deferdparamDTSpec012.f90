!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/12/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters: defined during
!                               intrinisic assignment; use poly-allocatable
!                               components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract:: base
    end type

    type container
        class(base), allocatable :: data(:)
    end type
end module

module m1
use m, only: base
    type, extends(base) :: child (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n) = 0.0
    end type
end module

program deferdparamDTSpec012
use m
use m1
    type (container), allocatable :: co1(:)
    logical(4), external :: precision_r8

    allocate (co1(100))

    !! assgn values for the 1st 20 elements
    do i = 1, 20
        allocate (child(8, 10+i) :: co1(i)%data(i))

        select type (x => co1(i)%data)
            type is (child(8,*))
                do j = 1, i
                    x(j)%data = (/(j*1.0d2+k, k=1,10+i)/)
                end do
            class default
                error stop 1_4
        end select
    end do

    !! test scalar assgn
    co1(70) = co1(20)

    !! test array assgn
    co1(71:89) = co1(19:1:-1)

    !! verify results
    do i = 70, 89
        select type (x => co1(i)%data)
            type is (child(8,*))
                do j = 1, 90-i
                    do k = 1, 100-i
                        if (.not. precision_r8(x(j)%data(k), &
                            j*1.0d2+k)) error stop 2_4
                    end do
                end do
            class default
                error stop 3_4
        end select
    end do
end
