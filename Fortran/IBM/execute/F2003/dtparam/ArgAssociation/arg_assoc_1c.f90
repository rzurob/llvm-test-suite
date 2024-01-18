!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/arg_assoc_1c.f
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
    integer :: a(n)
end type

type, extends(base) :: dt
    type(base(n)) :: bcomp1
    type(base(n+1)) :: bcomp2
    type(base(n+2)) :: bcomp3
    type(base(n+3)) :: bcomp4
    integer :: b(n+4)
end type

type(dt(4)) :: x
x%a = [(i,i=1,4)]
x%b = [(i,i=11,18)]
x%bcomp3%a = [(i,i=21,26)]

call sub(x,x%n)
print *,x%n,'-',x%a,'-',x%b,x%bcomp3%a

contains
    subroutine sub(x,n)
        integer, intent(in) :: n
        type(dt(n)) :: x,y
        print *,x%n,'-',x%a,'-',x%b,x%bcomp3%a
		x%bcomp3%a = [(i,i=31,36)]
        y = x
        print *,y%n,'-',y%a,'-',y%b,x%bcomp3%a
    end subroutine
end
