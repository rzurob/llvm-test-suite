!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/09/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Use array sections as the data target in the
!                               structure constructor for pointer component;
!                               data target is a dummy-arg.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k, dim)
        integer, kind :: k
        integer, len :: dim

        real(k) :: x(dim)
    end type

    type, extends(point) :: colorPoint
        integer color
    end type

    type base(k)
        integer, kind :: k

        class(point(k,:)), pointer :: points(:)
    end type

    contains

    subroutine associatePtr8 (b, p)
        type(base(8)), intent(out) :: b
        type(point(8,*)), target, intent(inout) :: p(:)

        b = base(8)(p)

        do i = 1, size(p)
            b%points(i)%x = (/(j, j = i, i+2)/)
        end do
    end subroutine
end module

program dtparamConstr047
use m
    type(colorPoint(8,3)), target :: cp(0:9)

    type(base(8)) b1

    logical(4), external :: precision_r8

    call associatePtr8 (b1, cp(::2)%point)

    if (.not. same_type_as(b1%points, cp(1)%point)) error stop 1_4

    if (size(b1%points) /= 5) error stop 2_4

    if (.not. associated(b1%points, cp(::2)%point)) error stop 3_4

    if (b1%points%dim /= 3) error stop 4_4

    do i = 0, 9, 2
        do j = 1, 3
            if (.not. precision_r8(cp(i)%x(j), (j+i/2)*1.0d0)) error stop 5_4
        end do
    end do
end
