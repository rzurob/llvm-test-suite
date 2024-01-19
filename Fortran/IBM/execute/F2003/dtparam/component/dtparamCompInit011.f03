! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/07/2009
!*
!*  DESCRIPTION                : module names are global identifier, type
!                               parameter names are of local identifier in class
!                               2. There is no name clash in any cases between a
!                               module name and a type parameter name.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module n
    integer, parameter :: const = 100
    type X (m,n)
        integer, kind :: m, n

        integer :: i(m,n)
        integer :: j = const -m -n
    end type

end module

module m
use n
    type Y (m,n)
        integer, kind :: m,n

        type(X(m,n)) :: x1 = X(m,n)(mod(m, n))
        integer :: y1 = mod(m*n, const)
    end type
end module

use m, only: y
use n, only : x, const
    implicit none

    type (Y(1,2)) y1

    type (Y(2,3)) y2

    print *, y1
    print *, y2

    if (any(shape(y1%x1%i) /= (/1,2/))) error stop 1_4

    if (any(shape(y2%x1%i) /= (/2,3/))) error stop 2_4

    if (any(y1%x1%i /= 1)) error stop 3_4

    if (any(y2%x1%i /= 2)) error stop 4_4

    if ((y1%y1 /= 2) .or. (y2%y1 /= 6)) error stop 5_4

    if ((y1%x1%j /= const - 3) .or. (y2%x1%j /= const - 5)) error stop 6_4
end
