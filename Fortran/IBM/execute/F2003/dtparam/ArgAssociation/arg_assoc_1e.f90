!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/arg_assoc_1e.f
!*  DATE                       : Jan. 19, 2009
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 361232
!*
!*  DESCRIPTION:
!*
!*  12.4.1 1 Actual arguments, dummy arguments, and argument association
!*
!234567890123456789012345678901234567890123456789012345678901234567890

type base(n)
    integer, len :: n
    character(n) :: a(n)
end type

type, extends(base) :: dt
    integer :: b(n)
end type

type(dt(3)) :: x
x%a = ['a12','b45','c78']
x%b = [(i,i=1,3)]

call sub(x%n)

contains
    subroutine sub(n)
        integer, intent(in) :: n
        type(dt(n)), allocatable :: y
        print *,x%a,'-',x%b
		y = x
        print *,y%a,'-',y%b
    end subroutine
end
