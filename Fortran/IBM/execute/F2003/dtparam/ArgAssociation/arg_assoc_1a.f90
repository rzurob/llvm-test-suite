!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/arg_assoc_1a.f
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

module m
    type base(k,n)
        integer, kind :: k
        integer, len :: n
        integer(k) :: b(n)
        integer(k) :: a(n)
    end type
end module

use m
type(base(8,:)), allocatable :: x
allocate(base(8,8) :: x)
x%a = [(i,i=1,8)]
x%b = [(i,i=11,18)]

call sub(x,x%n)

contains
    subroutine sub(x,n)
        use m
        integer, intent(in) :: n
        type(base(8,n)) :: x
        print *,x%a
        print *,x%b
    end subroutine
end
