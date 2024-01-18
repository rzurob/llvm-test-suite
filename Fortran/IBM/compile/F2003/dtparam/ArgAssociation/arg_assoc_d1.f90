!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/arg_assoc_d1.f
!*  DATE                       : Jan. 19, 2009
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
        integer :: b(n)
        integer :: a(n)
    end type
end module

use m
type(base(4,:)), allocatable, target :: x
type(base(4,:)), pointer :: p
allocate(base(4,8) :: x)
x%b = [(i,i=11,18)]
x%a = [(i,i=1,8)]

call sub_1(n=x%n,x=x)
call sub_2(y=x,x=x)
p => x
call sub_3(p,x)

contains
    subroutine sub_1(x,n)
        use m
        integer, intent(in), optional :: n
        type(base(4,n)) :: x
        print *,x%a
    end subroutine

    subroutine sub_2(x,y)
        use m
        type(base(4,*)), optional :: x
        type(base(x%k,x%n)) :: y
        print *,y%a
    end subroutine

    subroutine sub_3(p,y)
        use m
        type(base(4,:)), pointer, optional :: p
        type(base(p%k,p%n)), optional :: y
        print *,y%a
        print *,y%b
    end subroutine

end
