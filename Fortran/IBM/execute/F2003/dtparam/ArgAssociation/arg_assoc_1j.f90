!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/arg_assoc_1j.f
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
        integer(k), allocatable :: a(:)
        character(n*2) :: b
        integer(k) :: c(n*3)
    end type
end module

use m
type(base(8,:)), allocatable :: x,y
allocate(base(8,3) :: x,y)
allocate(x%a(3))
x%a = [(i,i=1,3)]
x%b = 'abcdef'
x%c = [(i,i=11,19)]

y%a = [(i,i=11,13)]
y%b = 'klmnop'
y%c = [(i,i=21,29)]

call sub(x,y,x%n)

contains
    subroutine sub(x,y,n)
        use m
        integer, intent(in) :: n
        type(base(8,n)), target :: x
        type(base(8,*)), target :: y
        type(base(8,n)), pointer :: p1
        type(base(8,:)), pointer :: p2
        p1 => x
        print *,p1%a,p1%b,p1%c
        p2 => x
        print *,p2%a,p2%b,p2%c
        p2 => p1
        print *,p2%a,p2%b,p2%c
        p1 => y
        print *,p1%a,p1%b,p1%c
    end subroutine
end
