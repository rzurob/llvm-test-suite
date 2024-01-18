!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/d361313_3.f
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : Feb. 20, 2009
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 361232, defect 361313 (seq 4)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

type base(n)
    integer, len :: n
    integer :: a(n)
end type

type dta(n)
    integer, len :: n
    type(base(n)) :: dta1
    type(base(n)) :: dta2
end type

type(dta(4)) :: x

x%dta1 = base(4)(([(i,i=1,4)]))
x%dta2 = base(4)(([(i,i=11,14)]))
print *,'in main:'
print *,'x%dta1=',x%dta1
print *,'x%dta2=',x%dta2

call sub(x,x%n)

contains
    subroutine sub(d,n)
        integer, intent(in) :: n
        type(dta(n)) :: d
        print *,'in sub:'
        print *,'d%dta1=',d%dta1
        print *,'d%dta2  =',d%dta2
        print *,'d%dta2%a=',d%dta2%a
    end subroutine
end
