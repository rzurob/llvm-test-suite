! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/05/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (A test case on overriding
!                               binding using dummy-procedure.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) data(n)

        contains

        procedure :: select => getDataWithCond
    end type

    abstract interface
        logical function compare4 (b1, b2)
            import
            class(base(4,1)), intent(in) :: b1, b2
        end function

        logical function compare8 (b1, b2)
        import
            class(base(8,1)), intent(in) :: b1, b2
        end function
    end interface

    contains

    function getDataWithCond (b1, comp, b2)
        class(base(4,*)), intent(in) :: b1
        class(base(4,1)), intent(in) :: b2
        procedure(compare4) comp

        class(base(4,:)), pointer :: getDataWithCond

        type (base(4,1)) temp
        logical mask(b1%n)

        do i = 1, b1%n
            temp%data = b1%data(i)

            mask(i) = comp(temp, b2)
        end do

        allocate (base(4, count(mask)) :: getDataWithCond)

        getDataWithCond%data = pack (b1%data, mask)
    end function
end module


module m1
use m
    type, extends(base) :: child
        logical(k) :: flags(n)

        contains

        procedure :: select => getChildDataWithCond
    end type

    contains

    function getChildDataWithCond (b1, comp, b2)
        class(child(4,*)), intent(in) :: b1
        class(base(4,1)), intent(in) :: b2
        procedure (compare4) comp

        class(base(4,:)), pointer :: getChildDataWithCond

        type (child(4,1)) temp
        logical mask (b1%n)

        do i = 1, b1%n
            temp%data(1) = b1%data(i)
            temp%flags(1) = b1%flags(i)

            mask(i) = comp (temp, b2)
        end do

        allocate (getChildDataWithCond, source = &
            child(4, count(mask))(pack(b1%data, mask), pack(b1%flags, mask)))
    end function
end module


program dtpOverride004
use m1
    class(base(4,:)), pointer :: b1

    logical, external :: precision_r4

    interface
        integer function testModM (b11)
        use m
            class(base(4,:)), pointer :: b11
        end function

        subroutine testModM1 (b11)
        use m1
            class(base(4,:)), pointer :: b11
        end subroutine
    end interface

    !! test1: test base type's select
    if (testModM (b1) /= 0) error stop 1_4

    if (b1%n /= 195+8) error stop 2_4

    do i = 1, 195
        if (.not. precision_r4 (b1%data(i), i+5.0_4)) error stop 3_4
    end do

    do i = 196, b1%n
        if (.not. precision_r4 (b1%data(i), log(197.0_4+i))) error stop 4_4
    end do

    deallocate (b1)

    !! test2: test child type's select
    call testModM1 (b1)

    if (.not. associated(b1)) error stop 6_4

    if (b1%n /= 97+4) error stop 7_4

    do i = 1, 97
        if (.not. precision_r4 (b1%data(i), 2.0_4*i + 5.0_4)) error stop 8_4
    end do

    do i = 98, b1%n
        if (.not. precision_r4 (b1%data(i), log(197.0 + i*2.0))) error stop 9_4
    end do

    select type (c => b1)
        type is (child(4,*))
            if (.not. all(c%flags)) error stop 10_4

        class default
            error stop 11_4
    end select
end


integer function testModM (b11)
use m
    class(base(4,:)), pointer :: b11

    class(base(4,:)), allocatable :: local

    procedure (compare4) b1GTb2

    allocate (base(4, 1000) :: local)

    local%data(:400) = -log([(i*1.0, i = 1, 400)])
!    local%data(601:) = - local%data(:400)
    local%data(601:) = log([(i*1.0, i = 1, 400)])
    local%data(401:600) = [(i, i = 1, 200)]

    b11 => local%select (b1GTb2, base(4,1)(log(392.1)))

    testModM = 0
end function


subroutine testModM1 (b11)
use m1
    class(base(4,:)), pointer :: b11

    class(base(4,:)), allocatable :: local

    procedure (compare4) c1GTc2

    allocate (local, source = child(4, 1000)(0, [(.true., .false., i=1,500)]))

    local%data(:400) = -log([(i*1.0, i = 1, 400)])
!    local%data(601:) = - local%data(:400)
    local%data(601:) = log([(i*1.0, i = 1, 400)])
    local%data(401:600) = [(i, i = 1, 200)]

    b11 => local%select (c1GTc2, child(4,1)(log(392.1), .true.))
end subroutine

logical function b1GTb2 (b1, b2)
use m
    class(base(4,1)), intent(in) :: b1, b2

    b1GTb2 = b1%data(1) > b2%data(1)
end function


logical function c1GTc2 (c1, c2)
use m1
    class(base(4,1)), intent(in) :: c1, c2

    c1GTc2 = .false.

    select type (c1)
        type is (child(4,*))
            select type (c2)
                type is (child(4,*))
                    c1GTc2 = (c1%data(1) > c2%data(1)) .and. &
                        (c1%flags(1) .eqv. c2%flags(1))

                class default
                    stop 50
            end select
        class default
            stop 100
    end select
end function
