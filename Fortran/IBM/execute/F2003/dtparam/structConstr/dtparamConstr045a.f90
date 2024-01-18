!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/02/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               A function results in a select type construct;
!                               structure constructor in allocate statement as a
!                               source-expr.
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
        integer(2) :: color
    end type

    contains

    function genColorPoint8 (d1, c)
        real(8), intent(in) :: d1(:)
        integer, intent(in) :: c

        class(point(8,size(d1))), pointer :: genColorPoint8

        allocate (genColorPoint8, source=colorPoint(8,size(d1))(d1, c))
    end function
end module

program dtparamConstr045
use m
    double precision d1(10)

    logical(4), external :: precision_r8

    d1 = (/(sin(i*1.0d0), i=1,10)/)

    select type (x => genColorPoint8 (d1, 10))
        class is (colorPoint(8,*))
            if (x%dim /= 10) error stop 2_4

            do i = 1, 10
                if (.not. precision_r8(x%x(i), sin(i*1.0d0))) error stop 3_4
            end do

            if (x%color /= 10) error stop 4_4

        class default
            error stop 1_4

    end select
end
