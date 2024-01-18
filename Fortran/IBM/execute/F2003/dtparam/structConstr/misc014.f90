!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/02/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 323729)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point4_25! (k, dim)
        integer :: dim = 25

        real(4) :: x(25)

        contains

        procedure, nopass :: typeID => pointTypeID
    end type

    type point8_20! (k, dim)
        integer :: dim = 20

        real(8) :: x(20)

        contains

        procedure, nopass :: typeID => pointTypeID
    end type

    type point16_20! (k, dim)
        integer :: dim = 20

        real(16) :: x(20)

        contains

        procedure, nopass :: typeID => pointTypeID
    end type

    type, extends(point8_20) :: colorPoint8_20
        integer(1) :: color

        contains

        procedure, nopass :: typeID => colorPointTypeID
    end type

    type base4! (k)
!        integer, kind :: k

        integer(min(4,4)) :: id
        class(point4_25), pointer :: points(:) => null()
    end type

    type base8! (k)
!        integer, kind :: k

        integer(min(8,4)) :: id
        class(point8_20), pointer :: points(:) => null()
    end type

    type base16! (k)
!        integer, kind :: k

        integer(min(16,4)) :: id
        class(point16_20), pointer :: points(:) => null()
    end type

    contains

    elemental integer function pointTypeID (i)
        integer, intent(in) :: i

        pointTypeID = 1
    end function

    elemental integer function colorPointTypeID (i)
        integer, intent(in) :: i

        colorPointTypeID = 2
    end function
end module

program dtparamConstr044
use m
    logical(4), external :: precision_r4, precision_r8

    type (point4_25), target :: p1(10)
    class (point8_20), allocatable, target :: p2(:)

    type(base4), pointer :: b1
    class(base8), allocatable :: b2(:)
    class(base16), pointer :: b3(:)

    p1 = (/(point4_25(x=i), i=1,10)/)

    allocate (p2(10), source=(/(colorPoint8_20(x=(/(i*100+j, j=1,20)/), &
            color=i), i=1,10)/))

    allocate (b1, b3(5))

    b1 = base4(id=100, points=p1)

    allocate (b2(10), source=(/(base8(id=i, points=p2), i=1,10)/))

    !! verify the results
    if (b1%id /= 100) error stop 1_4

    if (b1%points%typeID(b1%id) /= 1) error stop 2_4

    if (.not. associated(b1%points, p1)) error stop 12_4

    if (b1%points(1)%dim /= 25) error stop 13_4

    do i = 1, 10
        do j = 1, 25
            if (.not. precision_r4(b1%points(i)%x(j), i*1.0)) error stop 14_4
        end do
    end do

    if (any(b2%id /= (/(j, j=1,10)/))) error stop 3_4

    do i = 1, 10
        if (b2(i)%points%typeID (-100) /= 2) error stop 4_4

        if (size(b2(i)%points%typeID((/(j, j=1,0)/))) /= 0) error stop 5_4

        if (.not. associated(b2(i)%points, p2)) error stop 6_4

        if (b2(i)%points(1)%dim /= 20) error stop 7_4

        do j = 1, 10
            do k = 1, 20
                if (.not. precision_r8(b2(i)%points(j)%x(k), &
                    real(j*100+k, 8))) error stop 8_4
            end do
        end do
    end do

    do i = 1, 5
        if (associated(b3(i)%points)) error stop 9_4

        if (b3(i)%points%typeID(i) /= 1) error stop 10_4
    end do
end
