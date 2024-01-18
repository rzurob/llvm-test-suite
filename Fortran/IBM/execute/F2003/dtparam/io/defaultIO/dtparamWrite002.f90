!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/04/2007
!*
!*  DESCRIPTION                : dtparam (IO)
!                               list-directed write on drived type with deferred
!                               type parameter.  Auto-reallocation is done due
!                               to intrinsic assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k,n)
        integer, kind :: k = 4
        integer, len :: n = 10

        real(k) data(n)
    end type

    contains

    function genBase (val)
        real(4), intent(in) :: val(:)

        type(base(4,size(val))) genBase

        genBase%data = val
    end function
end module

use m
    type(base(4, :)), allocatable :: b1

    print *, genBase ([(i*1.0, i=1,3)])

    b1 = genBase ([1.3, 2.4, 5.2, 1.9])

    print *, b1
    print *, b1%data

    b1 = genBase ([b1%data*2.0, b1%data])

    print *, b1
    print *, b1%data
end
