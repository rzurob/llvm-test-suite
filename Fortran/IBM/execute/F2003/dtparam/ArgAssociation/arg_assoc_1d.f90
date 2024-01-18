!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/arg_assoc_1d.f
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : Jan. 19, 2009
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
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

type dta(n)
    integer, len :: n
    type(base(n)) :: dta1
    type(base(n+1)) :: dta2
end type

type(dta(4)) :: x
type(dta(:)), allocatable :: y
allocate(dta(4) :: y)

x%dta1 = base(4)(([(i,i=1,4)]))
x%dta2 = base(5)(([(i,i=11,15)]))
print *,'x - in main:'
print *,'x%dta1=',x%dta1
print *,'x%dta2=',x%dta2

call sub(x,x%n)

y%dta1 = base(4)(([(i,i=1,4)]))
y%dta2 = base(5)(([(i,i=11,15)]))
print *,'y - in main:'
print *,'y%dta1=',y%dta1
print *,'y%dta2=',y%dta2

call sub(y,y%n)

contains
    subroutine sub(d,n)
        integer, intent(in) :: n
        type(dta(n)) :: d
        print *,'in sub:'
        print *,'d%dta1=',d%dta1
        print *,'d%dta2=',d%dta2
    end subroutine
end
