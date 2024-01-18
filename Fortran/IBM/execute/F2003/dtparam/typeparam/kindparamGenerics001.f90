!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/01/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Statement: Kind type parameters participate the
!                               generic resolution.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k)
        integer, kind :: k = 4

        real(k) :: x, y
    end type

    interface movePoint
        subroutine movePoint4 (p, to)
            import point
            type(point(4)), intent(out) :: p
            type(point(4)), intent(in) :: to
        end subroutine

        subroutine movePoint8 (p, to)
            import point
            type(point(8)), intent(out) :: p
            type(point(8)), intent(in) :: to
        end subroutine
    end interface movePoint
end module

program kindparamGenerics001
use m
    type (point) :: p1 = point(1.0, 4.0)
    type (point(8)), allocatable :: p2(:)

    logical(4), external :: precision_r4, precision_r8

    allocate (p2(100))

    call movePoint(p1, point(1.3e0, 3.1e0))
    call movePoint(p2(1), point(8)(2.15d0, 2.3d77))
    call movePoint(p2(10), p2(1))

    !! verify results
    if ((.not. precision_r4(p1%x, 1.3e0)) .or. &
        (.not. precision_r4(p1%y, 3.1e0))) error stop 1_4

    if ((.not. precision_r8(p2(1)%x, 2.15d0)) .or. &
        (.not. precision_r8(p2(1)%y, 2.3d77))) error stop 2_4

    if ((.not. precision_r8(p2(10)%x, 2.15d0)) .or. &
        (.not. precision_r8(p2(10)%y, 2.3d77))) error stop 3_4
end


subroutine movePoint4 (p, to)
use m, only: point
    type(point(4)), intent(out) :: p
    type(point(4)), intent(in) :: to

    p = to
end subroutine

subroutine movePoint8 (p, to)
use m, only: point
    type(point(8)), intent(out) :: p
    type(point(8)), intent(in) :: to

    p = to
end subroutine
