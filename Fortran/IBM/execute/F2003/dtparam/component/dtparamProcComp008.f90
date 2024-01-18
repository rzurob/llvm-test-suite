! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/24/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Test PASS(arg-name) for procedure pointer
!                               component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real :: data(n)

        procedure(getAll), pointer, pass(b2) :: all => null()
    end type

    contains

    function getAll (b1, b2, b3)
        real, pointer, dimension(:) :: getAll
        class (base(*)), intent(in) :: b1, b2, b3

        allocate(getAll(b1%n+b2%n+b3%n), source=(/b1%data, b2%data, b3%data/))
    end function
end module

program dtparamProcComp008
use m
    real, allocatable :: r1(:)
    real, pointer :: r2(:)

    logical(4), external :: precision_r4

    class(base(:)), allocatable :: b1(:), b2, b3

    allocate(base(10) :: b1(5))

    do i = 1, 5
        b1(i)%data = (/(i*1.0e1+j, j=0, 9)/)
        b1(i)%all => getAll
    end do

    r2 => b1(2)%all (b1(1), b1(3))

    allocate (b2, source=base(3)((/1.0, 2.0, 3.0/), null()))
    allocate (b3, source=base(2)((/1.0e2, 1.0e3/)))

    allocate (r1(15), source=b1(5)%all(b2, b3))

    !! verify r1 and r2
    if (size(r2) /= 30) error stop 1_4

    do i = 1, 30
        if (.not. precision_r4(r2(i), 9.0+i*1.0)) error stop 2_4
    end do

    do i = 1, 3
        if (.not. precision_r4(r1(i), i*1.0)) error stop 3_4
    end do

    do i = 4, 13
        if (.not. precision_r4(r1(i), i*1.0+4.6e1)) error stop 4_4
    end do

    if ((.not. precision_r4(r1(14), 1.0e2)) .or. &
        (.not. precision_r4(r1(15), 1.0e3))) error stop 5_4
end
