!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/arg_assoc_1b.f
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

type(base(4)) :: x
x%a = [(i,i=1,4)]

call sub(x,x%n)

contains
    subroutine sub(x,n)
        integer, intent(in) :: n
        type(base(n)) :: x,y
        print *,x%n
        print *,x%a
        y = x
        print *,y%n
        print *,y%a
    end subroutine
end
