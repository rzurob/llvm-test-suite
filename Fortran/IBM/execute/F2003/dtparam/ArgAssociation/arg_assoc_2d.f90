!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/arg_assoc_2d.f
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
    integer :: a(n) = 0
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

call sub(x,y)

y%dta1 = base(4)(([(i,i=1,4)]))
y%dta2 = base(5)(([(i,i=11,15)]))
print *,'y - in main:'
print *,'y%dta1=',y%dta1
print *,'y%dta2=',y%dta2

call sub(y,x)

contains
    subroutine sub(d1,d2)
        type(dta(*)) :: d1
        type(dta(d1%n)) :: d2
        print *,'in sub:'
        print *,'d1%dta1=',d1%dta1
        print *,'d1%dta2=',d1%dta2
        print *,'d2%dta1=',d2%dta1
        print *,'d2%dta2=',d2%dta2
    end subroutine
end
