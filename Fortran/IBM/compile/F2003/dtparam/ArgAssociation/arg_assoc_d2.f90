!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/arg_assoc_d2.f
!*  DATE                       : Jan. 19, 2009
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 361232
!*
!*  DESCRIPTION:
!*
!*  12.4.1.2
!*  An actual argument associated with a dummy argument that is allocatable or
!*  a pointer shall have deferred the same type parameters as the dummy
!*  argument.
!*
!*  If the dummy argument is a pointer, the actual argument shall be a pointer
!*  and the nondeferred type parameters and ranks shall agree. If a dummy
!*  argument is allocatable, the actual argument shall be allocatable and the
!*  nondeferred type parameters and ranks shall agree.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(k,x,y)
        integer, kind :: k
        integer, len :: x,y
        integer :: a(x)
        integer :: b(y)
    end type
end module

use m
type(base(4,:,20)), allocatable :: a
type(base(8,:,10)), pointer :: b
type(base(8,:,10)), allocatable :: c
type(base(8,20,20)), pointer :: d
type(base(4,:,20)), pointer :: e
call sub1(a)
call sub2(b)
call sub1(b)
call sub2(a)
call sub1(c)
call sub2(d)
call sub1(e)

contains
    subroutine sub1(dtobj)
        use m
        type(base(4,:,20)), allocatable :: dtobj
    end subroutine

    subroutine sub2(dtobj)
        use m
        type(base(8,:,10)), pointer :: dtobj
    end subroutine
end
