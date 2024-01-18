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
!*  DATE                       : 07/06/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound procedure (A test case
!                               testing the projected point algorithms. 2D and
!                               3D points are tested)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module pointMod
    type point (n)
        integer, len :: n = 2
        real coord(n)

        contains

        procedure :: length => lengthBetween
    end type

    contains

    real function lengthBetween (p1, p2)
        class(point(*)), intent(in) :: p1, p2

        if (p1%n /= p2%n) stop 10

        lengthBetween = sqrt(sum((p1%coord - p2%coord)**2))
    end function
end module

module projectedPointMod
use pointMod
    type, extends(point) :: projPoint
        contains

        procedure :: direction => getDirection
        procedure :: project => pointInDistance
    end type

    contains

    function getDirection (p1)
        class(projPoint(*)), intent(in) :: p1

        type (projPoint(p1%n)) getDirection

        real r

        getDirection = p1

        r = p1%length(projPoint(p1%n)(0.0))

        getDirection%coord = getDirection%coord / r
    end function

    function pointInDistance (p1, dr)
        class (projPoint(*)), intent(in) :: p1
        real, intent(in) :: dr

        type (projPoint(p1%n)) pointInDistance

        r = p1%length(projPoint(p1%n)(0.0)) + dr

        if (r <= 0.0) stop 20

        pointInDistance = p1%direction()

        pointInDistance%coord = pointInDistance%coord * r
    end function
end module

program testProjPoint
use projectedPointMod
    implicit none
    type(projPoint(:)), allocatable :: p1(:), p2(:)

    type(projPoint(2)), parameter :: origin2D = projPoint(2)(0.0)
    type(projPoint(3)), parameter :: origin3D = projPoint(3)(0.0)

    integer :: density

    real, parameter :: pi = 4.0*atan(1.0)

    integer i
    logical(4), external :: precision_r4

    type(projPoint(:)), allocatable :: x

    read (*,*) density

    !! test 1: 2-dimensional points
    allocate (projPoint(2) :: p1(density), p2(density))

    do i = 1, density
        p1(i)%coord = [cos(2.0*i*pi/density), sin(2.0*i*pi/density)]

        p2(i) = p1(i)%project(p1(i)%length(origin2D))
    end do

    !! now verify the results
    do i = 1, density
        if (.not. precision_r4(p2(i)%coord(1), 2.0*cos(2.*i*pi/density))) &
                error stop 1_4

        if (.not. precision_r4(p2(i)%coord(2), 2.0*sin(2.*i*pi/density))) &
                error stop 2_4
    end do


    !! test2: 3-D points
    deallocate (p1)

    allocate (projPoint(3) :: p1(density))

    do i = 1, density
!        associate (x => p2(i)%direction())
        x = p2(i)%direction()
        p1(i) = projPoint(3)([x%coord, 1.0])
!        end associate
    end do

    deallocate (p2)

    allocate (projPoint(3) :: p2(density))

    do i = 1, density
        p2(i) = p1(i)%project (p1(i)%length(origin3D))
    end do

    !! verify p2
    do i = 1, density
        if (.not. precision_r4 (p2(i)%coord(1), 2.0*cos(2.*i*pi/density))) &
                error stop 3_4

        if (.not. precision_r4 (p2(i)%coord(2), 2.0*sin(2.*i*pi/density))) &
                error stop 4_4

        if (.not. precision_r4 (p2(i)%coord(3), 2.0)) error stop 5_4
    end do
end
