! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/02/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Poly pointer array component in a structure's
!                               association by a (non)poly-data target array;
!                               array case
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k, dim)
        integer, len :: dim
        integer, kind :: k

        real(k) :: x(dim)

        contains

        procedure, nopass :: typeID => pointTypeID
    end type

    type, extends(point) :: colorPoint
        integer(1) :: color

        contains

        procedure, nopass :: typeID => colorPointTypeID
    end type

    type base (k)
        integer, kind :: k

        integer(min(k,4)) :: id
        class(point(k,:)), pointer :: points(:) => null()
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

    type (point(4, 25)), target :: p1(10)
    class (point(8, :)), allocatable, target :: p2(:)

    type(base(4)), pointer :: b1
    class(base(8)), allocatable :: b2(:)
    class(base(16)), pointer :: b3(:)

    p1 = (/(point(4,25)(x=i), i=1,10)/)

    allocate (p2(10), source=(/(colorPoint(8,20)((/(i*100+j, j=1,20)/), &
            color=i), i=1,10)/))

    allocate (b1, b3(5))

    b1 = base(4)(100, points=p1)

    allocate (b2(10), source=(/(base(8)(id=i, points=p2), i=1,10)/))

    !! verify the results
    if (b1%id /= 100) error stop 1_4

    if (b1%points%typeID(b1%id) /= 1) error stop 2_4

    if (.not. associated(b1%points, p1)) error stop 12_4

    if (b1%points%dim /= 25) error stop 13_4

    do i = 1, 10
        do j = 1, 25
            if (.not. precision_r4(b1%points(i)%x(j), i*1.0)) error stop 14_4
        end do
    end do

    if (any(b2%id /= (/(j, j=1,10)/))) error stop 3_4

    do i = 1, 10
        if (b2(i)%points%typeID (-100) /= 2) error stop 4_4

        if (size(b2(i)%points%typeID((/integer::/))) /= 0) error stop 5_4

        if (.not. associated(b2(i)%points, p2)) error stop 6_4

        if (b2(i)%points%dim /= 20) error stop 7_4

        do j = 1, 10
            do k = 1, 20
                if (.not. precision_r8(b2(i)%points(j)%x(k), &
                    real(j*100+k, 8))) error stop 8_4
            end do
        end do

        select type (x => b2(i)%points)
            type is (colorPoint(8,*))
                do j = 1, 10
                    if (x(j)%color /= j) error stop 21_4
                end do

            class default
                error stop 20_4
        end select
    end do

    do i = 1, 5
        if (associated(b3(i)%points)) error stop 9_4

        if (b3(i)%points%typeID(i) /= 1) error stop 10_4
    end do
end
