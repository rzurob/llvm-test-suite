! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/14/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Diagnostic case for kind
!                               type parameter mis-match in the invocation.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k, dim)
        integer, kind :: k, dim

        real(k) :: coor (dim)

        contains

        procedure :: length
    end type

    contains

    real(8) function length (p1, p2)
        class(point(8,2)), intent(in) :: p1, p2

        length = 0
    end function
end module

program dtpPass001d
use m
    class(point(8,3)), allocatable :: p1
    type(point(4,2)) p2

    type(point(8,2)) p3

    print *, p3%length(p2)      !<-- illegal

    r1 = p2%length(p3)          !<-- illegal

    r1 = p1%length(p2)          !<-- illegal
end
