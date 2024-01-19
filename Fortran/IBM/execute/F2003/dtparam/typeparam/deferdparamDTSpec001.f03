! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/10/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: common use in components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real(8) :: data(n)
    end type

    type container
        class(base(:)), allocatable :: b1(:)
        type (base(:)), pointer :: b2 => null()
    end type
end module

program deferdparamDTSpec001
use m
    class (container), pointer :: co1

    logical(4), external :: precision_r8

    allocate (co1)

    allocate (base(n=100) :: co1%b1(10))
    allocate (base(10) :: co1%b2)

    co1%b1(10)%data = (/(i*1.0d0, i=1,100)/)
    co1%b2%data = (/(i+1.0d2, i=1, 10)/)

    !! verify results
    do i = 1, 100
        if (.not. precision_r8(co1%b1(10)%data(i), i*1.0d0)) error stop 1_4
    end do

    do i = 1, 10
        if (.not. precision_r8(co1%b2%data(i)-1.0d2, i*1.0d0)) error stop 2_4
    end do
end
