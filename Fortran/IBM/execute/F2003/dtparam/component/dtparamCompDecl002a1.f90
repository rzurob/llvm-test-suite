!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 01/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Make dtparamCompDecl002a more straightforward,
!                               i.e. not to use 2-dimensional array for the
!                               line%l1, instead use an array of parameterized
!                               points.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (ndim)
        integer, len :: ndim

        real :: coord(ndim)
    end type

    type line (ndim, n)
        integer, len :: ndim = 2, n

        !! for ndim <= 2, it's a 2-dimensional line containing n points
        !! for ndim >= 3, it's a 3-dimensional line containing n points
        type(point(min(dim(ndim, 2), 1)+2)) , dimension(n) :: points
    end type

    type shape (n)
        integer, len :: n

        type(line(:,n)), allocatable :: l1
    end type

    contains

    function convert2Points (l1)
        type(line(*, *)), intent(in) :: l1

        type (point(min(dim(l1%ndim, 2), 1)+2)), dimension(l1%n) :: convert2Points

        convert2Points = l1%points
    end function
end module

program dtparamCompDecl002a1
use m
    type(shape(:)), allocatable :: s1(:)

    type (point(:)), allocatable :: p1(:)

    logical(4), external :: precision_r4

    integer, parameter :: ncount = 10

    allocate (shape(ncount) :: s1(2))
    allocate (line(1, ncount) :: s1(1)%l1)
    allocate (line(4, ncount) :: s1(2)%l1)

    do i = 1, ncount
        s1(1)%l1%points(i) = point(2)((/i*2.0-1.0, i*2.0/))

        s1(2)%l1%points(i) = point(3)((/i*3.e1-2.e1, i*3.e1-1.e1, i*3.e1/))
    end do

    allocate (p1(ncount), source = convert2Points(s1(1)%l1))


    !! verify results
    do i = 1, 10
        if (.not. precision_r4(p1(i)%coord(1), 2.0*i-1.0)) error stop 1_4
        if (.not. precision_r4(p1(i)%coord(2), 2.0*i)) error stop 2_4
    end do

    deallocate (p1)

    allocate (p1(ncount), source = convert2Points(s1(2)%l1))

    !! verify results
    do i = 1, 10
        if (.not. precision_r4(p1(i)%coord(1), 3.0e1*i -2.0e1)) error stop 3_4
        if (.not. precision_r4(p1(i)%coord(2), 3.0e1*i -1.0e1)) error stop 4_4
        if (.not. precision_r4(p1(i)%coord(3), 3.0e1*i)) error stop 5_4
    end do

    deallocate (p1, s1)
end
