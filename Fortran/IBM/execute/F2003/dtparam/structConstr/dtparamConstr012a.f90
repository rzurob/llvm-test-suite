! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/27/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Components with default initializations
!                               need not to be sipplied by the component-spec in
!                               structure constructor.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k, dim)
        integer, kind :: k, dim

        real(k) :: coord(dim)
    end type

    type, extends(point) :: colorPoint (ck)
        integer, kind :: ck

        integer(ck) :: color = -1
    end type
end module

program dtparamConstr012a
use m
    class(point(8, 2)), allocatable :: p1
    type (colorPoint(4, 3, 1)) :: cp1

    class(point(8, 3)), pointer :: p2(:)

    logical(4), external :: precision_r4, precision_r8

    allocate (p1, source=colorPoint(8,2,2)(point=point(8,2)(1.2d0)))

    allocate (p2(0:9), source=(/(colorpoint(8,3,4)((/(i*1.0d2+j, j=1,3)/)), i=0,9)/))

    cp1 = colorPoint(4,3,1)(coord=(/1.4, 2.3, 5.1/))

    !! verify results
    if ((.not. precision_r8(p1%coord(1), 1.2d0)) .or. &
        (.not. precision_r8(p1%coord(2), 1.2d0))) error stop 1_4

    if ((.not. precision_r4(cp1%coord(1), 1.4e0)) .or. &
        (.not. precision_r4(cp1%coord(2), 2.3e0)) .or. &
        (.not. precision_r4(cp1%coord(3), 5.1e0))) error stop 2_4

    if (cp1%color /= -1) error stop 3_4

    do i = 0, 9
        do j = 1, 3
            if (.not. precision_r8(p2(i)%coord(j), i*1.0d2+j)) error stop 4_4
        end do
    end do

    select type (p1)
        type is (colorPoint(8,2,2))
            if (p1%color /= -1) error stop 5_4

        class default
            error stop 6_4
    end select

    select type (p2)
        class is (colorPoint(8,3,4))
            if (any(p2%color /= (/(-1, i=1, 10)/))) error stop 7_4

        class default
            error stop 8_4
    end select
end
