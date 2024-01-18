!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/14/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Test default initializations for the
!                               component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module pointMod
    type point (k, dim)
        integer, kind :: k, dim

        real(k) :: coord(dim) = (/(i, i=1, dim)/)
    end type

    integer black, red, yellow, blue

    parameter (black = 0, red = 1, yellow=2, blue=3)

    type, extends(point) :: colorPoint (ck)
        integer, kind :: ck

        integer(ck) :: color = black
    end type

    class (point(8,3)), allocatable :: p1(:)
end module

program dtparamDefInit004
use pointMod
    type (colorPoint(4,2,2)) :: cp1(3)

    logical(4), external :: precision_r4

    allocate (p1(10), source=(/(colorPoint(8,3,1)(point= &
            point(8,3)((/(real(j+i)/i, i=1,3)/))), j = 1, 10)/))


    cp1 = (/colorPoint(4,2,2)(color=red), colorPoint(4,2,2)(color=yellow), &
            colorPoint(4,2,2)(color=blue)/)


    !! verify p1 and cp1
    do i = 1, 10
        do j = 1, 3
            if (.not. precision_r4(real(p1(i)%coord(j), 4), (i+j)*1.0/j)) &
                    error stop 1_4
        end do
    end do

    select type (p1)
        type is (colorPoint(8,3,1))
            if (any(p1%color /= 0)) error stop 2_4

        class default
            error stop 3_4
    end select

    do i = 1, 3
        do j = 1, 2
            if (.not. precision_r4(cp1(i)%coord(j), j*1.0)) error stop 4_4
        end do
    end do

    if (any(cp1%color /= (/red, yellow, blue/))) error stop 5_4
end
