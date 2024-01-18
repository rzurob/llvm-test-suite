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
!*  Default initialization, automatic object, varying offset components
!*
!234567890123456789012345678901234567890123456789012345678901234567890

type base(n)
    integer, len :: n
    integer :: a(n+1) = 12
end type

type, extends(base) :: dt
    integer :: b(n+2) = 34
    integer :: c(n+3) = 56
    type(base(n)) :: dummy1
    type(base(n*2)) :: dummy2
    type(dt(n)), pointer :: next => null()
end type

type(dt(2)) :: x,y

call sub(x,y)

contains
    subroutine sub(x,y)
        type(dt(*)), intent(in) :: y
        type(dt(y%n)), intent(inout) :: x
        type(dt(size(y%a)-1)) :: z
        integer i
        print *,x%a,x%b,x%c,associated(x%next)
        print *,z%a,z%b,z%c,associated(z%next)
        z = x
        print *,z%a,z%b,z%c,associated(z%next)
    end subroutine
end
