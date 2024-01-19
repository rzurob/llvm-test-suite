!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 19, 2009
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 361232
!*
!*  DESCRIPTION:
!*
!*  12.4.1 1 Actual arguments, dummy arguments, and argument association
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(n)
        integer, len :: n
        integer :: b(n)
        integer :: a(n)
    end type

    type, extends(base) :: dt
    end type
end module

use m
type(dt(:)), allocatable :: x,y
allocate(dt(4) :: x)
allocate(dt(4) :: y)
x%a = [(i,i=1,4)]
x%b = [(i,i=11,14)]
y = x

call sub(x,y)

contains
    subroutine sub(x,y)
        use m
        type(dt(*)) :: x
        type(dt(x%n)) :: y
        type(dt(x%n)) :: z
        print *,x
        print *,y
        z = x
        print *,z%n, z
        z = y
        print *,z%n, z
    end subroutine
end
