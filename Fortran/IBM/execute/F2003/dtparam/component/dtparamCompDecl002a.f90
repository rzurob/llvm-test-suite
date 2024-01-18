! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/20/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Extends test case dtparamCompDecl002 to
!                               include 3-dimensional points.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        real :: x, y
    end type

    type, extends(point) :: point3D
        real :: z
    end type

    type line (ndim, n)
        integer, len :: ndim = 2, n

        !! for ndim <= 2, it's a 2-dimensional line containing n points
        !! for ndim >= 3, it's a 3-dimensional line containing n points
        real, dimension (min(dim(ndim, 2), 1)+2, n) :: coord
    end type

    type shape (n)
        integer, len :: n

        type(line(:,n)), allocatable :: l1
    end type

    contains

    function convert2Points (l1)
        type(line(*, *)), intent(in) :: l1

        class (point), dimension(:), allocatable :: convert2Points

        select case (size(l1%coord, 1))
            case (2)    !must be 2-dimensional points
                allocate (convert2Points(l1%n))
            case (3)
                allocate (point3D:: convert2Points(l1%n))
            case default
                stop 10
        end select

        do i = 1, l1%n
            convert2Points(i)%x = l1%coord(1,i)
            convert2Points(i)%y = l1%coord(2,i)
        end do

        select type (convert2Points)
            type is (point3D)
                do i = 1, l1%n
                    convert2Points(i)%z = l1%coord(3,i)
                end do
        end select
    end function
end module

program dtparamCompDecl002a
use m
    type(shape(:)), allocatable :: s1(:)

    class (point), allocatable :: p1(:)

    logical(4), external :: precision_r4

    allocate (shape(10) :: s1(2))
    allocate (line(1, 10) :: s1(1)%l1)
    allocate (line(4, 10) :: s1(2)%l1)

    s1(1)%l1%coord = reshape ((/(i*1.0, i=1, 20)/), (/2, 10/))
    s1(2)%l1%coord = reshape ((/(i*1.0e1, i=1, 30)/), (/3, 10/))

    allocate (p1(s1%n), source = convert2Points(s1(1)%l1))


    !! verify results
    do i = 1, 10
        if (.not. precision_r4(p1(i)%x, 2.0*i-1.0)) error stop 1_4
        if (.not. precision_r4(p1(i)%y, 2.0*i)) error stop 2_4
    end do

    deallocate (p1)

    allocate (p1(s1%n), source = convert2Points(s1(2)%l1))

    !! verify results
    do i = 1, 10
        select type (x => p1(i))
            type is (point3D)
                if (.not. precision_r4(x%x, 3.0e1*i -2.0e1)) error stop 3_4
                if (.not. precision_r4(x%y, 3.0e1*i -1.0e1)) error stop 4_4
                if (.not. precision_r4(x%z, 3.0e1*i)) error stop 5_4
            class default
                error stop 7_4
        end select
    end do

    deallocate (p1, s1)
end
