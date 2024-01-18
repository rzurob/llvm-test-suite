!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/20/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Type-parameter values for components can
!                               be specification expressions that contains
!                               neither specification function nor object
!                               designators that are not named constants or
!                               suboject thereof.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        real :: x, y
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
        type(line(1, *)), intent(in) :: l1

        type (point), dimension(l1%n) :: convert2Points

        do i = 1, l1%n
            convert2Points(i)%x = l1%coord(1,i)
            convert2Points(i)%y = l1%coord(2,i)
        end do
    end function
end module

program dtparamCompDecl002
use m
    type (shape(10)) s1
    type (point), allocatable :: p1(:)

    logical(4), external :: precision_r4

    allocate (line(1, 10) :: s1%l1)

    allocate (p1(10))

    s1%l1%coord = reshape ((/(i*1.0, i=1, 20)/), (/2, 10/))

    p1 = convert2Points(s1%l1)


    !! verify results
    do i = 1, 10
        if (.not. precision_r4(p1(i)%x, 2.0*i-1.0)) error stop 1_4
        if (.not. precision_r4(p1(i)%y, 2.0*i)) error stop 2_4
    end do
end
