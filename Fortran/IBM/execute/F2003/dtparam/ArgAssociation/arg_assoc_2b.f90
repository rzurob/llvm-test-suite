!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/arg_assoc_2b.f
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
end module

use m
type(base(:)), allocatable :: x,y
allocate(base(4) :: x)
allocate(base(4) :: y)
x%a = [(i,i=1,4)]
x%b = [(i,i=11,14)]
y = x

call sub(x,y)

contains
    subroutine sub(x,y)
        use m
        type(base(*)) :: x
        type(base(x%n)) :: y
        type(base(x%n)) :: z
        print *,x
        print *,y
        z = x
        print *,z
        z = y
        print *,z
    end subroutine
end
