! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/08/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Put the structure constructor as a selector in
!                               an associate construct; component is a pointer
!                               scalar.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k, dim)
        integer, kind :: k
        integer, len :: dim

        real(k) :: x(dim)
    end type

    type, extends(point) ::colorPoint
        integer(min(k,2)) :: color
    end type

    interface genPoint
        module procedure genPoint4
        procedure genColorPoint8
    end interface

    type base (k)
        integer, kind :: k

        class(point(k,:)), pointer :: point
    end type

    contains

    function genPoint4 (r1)
        real(4), intent(in) :: r1(:)

        class (point(4,:)), pointer :: genPoint4

        allocate (point(4,size(r1)):: genPoint4)

        genPoint4%x = r1
    end function

    function genColorPoint8 (d1, c)
        real(8), intent(in) :: d1(:)
        integer, intent(in) :: c

        class(point(8,size(d1))), pointer :: genColorPoint8

        allocate (genColorPoint8, source=colorPoint(8,size(d1))(d1, c))
    end function
end module

program dtparamConstr045
use m
    real(4) :: x(3)

    logical(4) precision_r4, precision_r8

    x = (/1.0, 2.1, -1.2/)

    !! 1st block: point of kind 4
    associate (x => base(4)(genPoint4(x)))
        if (.not. associated(x%point)) error stop 1_4

        if (x%point%dim /= 3) error stop 2_4

        if (.not. precision_r4(1.0, x%point%x(1))) error stop 3_4
        if (.not. precision_r4(2.1, x%point%x(2))) error stop 4_4
        if (.not. precision_r4(-1.2, x%point%x(3))) error stop 5_4

        if (.not. same_type_as(x%point, point(4,3)(1))) error stop 6_4
    end associate

    !! 2nd block: point of kind 8
    associate (x => base(8)(genColorPoint8 ((/1.1d0, 2.1d1/), c = 10)))
        if (.not. associated(x%point)) error stop 7_4

        if (x%point%dim /= 2) error stop 8_4

        if (.not. precision_r8(1.1d0, x%point%x(1))) error stop 9_4
        if (.not. precision_r8(2.1d1, x%point%x(2))) error stop 10_4

        select type (y => x%point)
            type is (colorPoint(8,*))
                if (y%color /= 10) error stop 11_4

            class default
                error stop 15_4
        end select
    end associate
end
