! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/31/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Deferred type-parameter value shall be
!                               specified in allocate statement; either by
!                               type-spec or source-expr.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real :: data(n)
    end type
end module

program dtSpec003d2
use m
    type (base(:)), allocatable :: b1(:)

    allocate (b1(100))      !<-- illegal: n is unknown here
    print *, b1%n
end
