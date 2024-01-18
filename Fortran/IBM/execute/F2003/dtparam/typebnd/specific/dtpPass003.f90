!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/16/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (A simple test that uses
!                               same type bound to different bindings.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module point_mod
    type point (k)
        integer, kind :: k

        real(k) :: x, y

        contains

        procedure, pass (p1) :: equal4 => p1EqualP2
        procedure, pass(p2) :: equal8  => p1EqualP2
    end type

    contains

    logical function p1EqualP2 (p1, p2)
        class(point(4)), intent(in) :: p1
        class(point(8)), intent(in) :: p2

        logical(4), external :: precision_r4

        p1EqualP2 = precision_r4(p1%x, real(p2%x,4)) .and. &
            precision_r4(p1%y, real(p2%y,4))
    end function
end module

program dtpPass003
use point_mod
    type(point(4)) p1
    type(point(8)), allocatable :: p2

    class(point(4)), pointer :: p3(:)

    class(point(8)), allocatable :: p4(:,:)

    p1 = point(4)(1.2, 2.3)
    p2 = point(8)(1.2, 2.3)

    if ((.not. p1%equal4(p2)) .or. (.not. p2%equal8(p1))) error stop 1_4

    allocate (p3(100), source=[(point(4)(sin(real(i)), cos(real(i))), i=1,100)])

    allocate (p4(10,10), source=reshape([(point(8)(p3(i)%x, p3(i)%y), &
            i=1,100)],[10,10]))


    k = 1

    do j = 1, 10
        do i = 1, 10
            if (.not. p4(i,j)%equal8(p3(k))) error stop 2_4

            if (.not. p3(k)%equal4(p4(i,j))) error stop 3_4

            k = k + 1
        end do
    end do
end
