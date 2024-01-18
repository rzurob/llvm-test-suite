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
!*  DATE                       : 05/04/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Nopass binding for using
!                               pointer dummy-arg.)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k, dim)
        integer, kind :: k
        integer, len :: dim

        real(k) :: coord(dim)

        contains

        procedure, nopass :: move => movePoint8
    end type

    contains

    subroutine movePoint8 (b1, delta)
        class(point(8,:)), pointer, intent(inout) :: b1
        real(8), intent(in) :: delta

        if (.not. associated(b1)) then
            return
        else
            b1%coord = b1%coord + delta
        end if
    end subroutine
end module

module m1
use m
    type, extends(point) :: colorPoint (ck)
        integer, kind :: ck

        integer(ck) :: color = 0

        contains

        procedure, nopass :: move => moveColorPoint8
    end type

    contains

    subroutine moveColorPoint8 (b1, delta)
        class(point(8,:)), pointer, intent(inout) :: b1
        real(8), intent(in) :: delta

        if (.not. associated(b1)) then
            return
        else
            call movePoint8 (b1, delta)
            select type (x => b1)
                type is (point(8,*))
!                    call x%move(x, delta)

                type is (colorPoint(8,*,4))
!                    call x%point%move(x%proint, delta)

                    x%color = x%color + 1

                class default
                    stop 10
            end select
        end if
    end subroutine
end module

program dtpNopass007a
use m1
    class(point(16,:)), allocatable :: p1
    class(point(8,:)), pointer :: p2

    logical(4), external :: precision_r8

    allocate (point(16,2) :: p1)

    nullify (p2)

    !! we have a disassociated pointer returned
    call p1%move(p2, 1.2d0)

    if (associated(p2)) error stop 1_4

    allocate (p2, source=point(8,3)([1,0,-1]))

    !! dynamic type of p2 is of point
    call p1%move(p2, 1.2d0)

    if (.not. associated(p2)) error stop 2_4

    if (.not. same_type_as(p2, point(8,3)(0))) error stop 3_4

    if ((.not. precision_r8(p2%coord(1), 2.2d0)) .or. &
        (.not. precision_r8(p2%coord(2), 1.2d0)) .or. & 
        (.not. precision_r8(p2%coord(3), 0.2d0))) error stop 4_4

    deallocate (p1, p2)

    allocate (colorPoint(16,2,2) :: p1)
    allocate (p2, source=colorPoint(8,3,4)([2.2d0,1.2d0,0.2d0], -1))

    !! dynamic type of p2 is colorPoint
    call p1%move(p2, 1.2d0)

    if (.not. associated(p2)) error stop 5_4

    if (.not. same_type_as(p2, colorPoint(8,3,4)(1))) error stop 6_4

    if ((.not. precision_r8(p2%coord(1), 3.4d0)) .or. &
        (.not. precision_r8(p2%coord(2), 2.4d0)) .or. &
        (.not. precision_r8(p2%coord(3), 1.4d0))) error stop 7_4

    select type (p2)
        class default
            error stop 8_4

        type is (colorPoint(8,*,4))
            if (p2%color /= 0) error stop 9_4
    end select

    !! dynamic type of p2 is colorPoint, with color value changed from default
    call p1%move(p2, -1.2d0)

    if (.not. same_type_as(p2, colorPoint(8,3,4)(1))) error stop 10_4

    if ((.not. precision_r8(p2%coord(1), 2.2d0)) .or. &
        (.not. precision_r8(p2%coord(2), 1.2d0)) .or. & 
        (.not. precision_r8(p2%coord(3), 0.2d0))) error stop 11_4

    select type (p2)
        class default
            error stop 12_4

        type is (colorPoint(8,*,4))
            if (p2%color /= 1) error stop 13_4
    end select

    deallocate (p2)
end
