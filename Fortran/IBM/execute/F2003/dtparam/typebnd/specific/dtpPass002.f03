! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/15/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Test case on elemental pass
!                               binding with length type parameter: point and
!                               its length calculation.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (dim)
        integer, len :: dim

        real coor(dim)

        contains

        procedure :: length
    end type

    contains

    elemental real function length (p1, p2)
        class(point(*)), intent(in) :: p1, p2

        length = sqrt(-1.0)

        if (p1%dim /= p2%dim) return

        length = 0

        do i = 1, p1%dim
            length = length + (p1%coor(i) - p2%coor(i))**2
        end do

        length = sqrt(length)
    end function
end module

program dtpPass002
use m
    type(point(:)), allocatable :: p1(:), p2(:)
    logical(4), external :: precision_r4

    real, allocatable :: len1(:), len2(:)

    p1 = [(point(3)([(j, j=i*100, i*100+2)]), i=1,5)]

    p2 = p1(5:1:-1)

    len1 = p1%length(point(3)(0))
    len2 = p1%length(p2)

    if ((.not. allocated(p1)) .or. (.not. allocated(p2))) error stop 1_4

    if ((size(p1) /= 5) .or. (size(p2) /= 5)) error stop 2_4

    if ((p1%dim /= 3) .or. (p2%dim /= 3)) error stop 3_4

    do i = 1, 5
        r1 = sqrt((i*1.00e2)**2 + (i*1.0e2+1)**2 + (i*1.0e2+2)**2)

        if (.not. precision_r4(r1, len1(i))) error stop 4_4

        r1 = sqrt(3.0)*abs(6-2*i)*1.0e2

        if (.not. precision_r4(r1, len2(i))) then
            if ((i == 3) .and. (abs(r1 - len2(i)) <= 1.0e-7)) then
                continue
            else
                error stop 5_4
            end if
        end if
    end do
end
