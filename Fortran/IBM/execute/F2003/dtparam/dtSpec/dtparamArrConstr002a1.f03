! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Put entities with pointer component in the
!                               array constructor.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type container (k, n)
        integer, kind :: k
        integer, len :: n

        class(base(k,n)), pointer :: b1 => null()
        procedure(genBase), pointer :: gen4 => null()
    end type

    abstract interface
        class(base(4,:)) function genBase (co)
        import
            class(container(4,*)), intent(in) :: co
            allocatable genBase
        end function
    end interface
end module

program dtparamArrConstr002a1
use m
    type (container(4,:)), allocatable :: co1(:)

    class(container(4,:)), pointer :: co2

    type (container(4, 55)) co3(3)

    logical(4), external :: precision_r4

    procedure(genBase) genBaseAlloc4, genBaseAllocDouble4

    allocate (container(4, 55):: co1(4), co2)

    co1(3)%gen4 => genBaseAlloc4
    co2%gen4    => genBaseAllocDouble4

    allocate (co1(3)%b1, source=base(4, 55)((/(i*1.0e0, i=1, 55)/)))
    allocate (co2%b1, source=base(4,55)(2.0))

    co3 = (/container(4, 55) :: co1(1::2), co2/)

    !! verify association status
    if (associated(co3(1)%gen4) .or. associated(co3(1)%b1)) error stop 1_4

    if ((.not. associated(co3(2)%b1, co1(3)%b1)) .or. &
        (.not. associated(co3(2)%gen4, genBaseAlloc4))) error stop 2_4

    allocate (co3(1)%b1, source=co3(2)%gen4())

    if ((.not. associated(co3(3)%b1, co2%b1)) .or. &
        (.not. associated(co3(3)%gen4, genBaseAllocDouble4))) error stop 3_4


    allocate (co1(4)%b1, source=co3(3)%gen4())

    !! verify data precision
    do i = 1, co3%n
        if (.not. precision_r4(co3(1)%b1%data(i), i*1.0e0)) error stop 4_4
        if (.not. precision_r4(co3(2)%b1%data(i), i*1.0e0)) error stop 5_4
        if (.not. precision_r4(co3(3)%b1%data(i), 2.0)) error stop 6_4
        if (.not. precision_r4(co1(4)%b1%data(i), 4.0)) error stop 7_4
    end do
end


class(base(4,:)) function genBaseAlloc4 (co)
    use m
    class(container(4,*)), intent(in) :: co

    allocatable :: genBaseAlloc4

    if (associated(co%b1)) then
        allocate(genBaseAlloc4, source=co%b1)
    end if
end function


class(base(4,:)) function genBaseAllocDouble4 (co)
    use m
    class(container(4,*)), intent(in) :: co
    allocatable genBaseAllocDouble4

    if (associated(co%b1)) then
        allocate (base(4, co%b1%n) :: genBaseAllocDouble4)

        genBaseAllocDouble4%data = 2.0*co%b1%data
    end if
end function
