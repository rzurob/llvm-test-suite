!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/arg_assoc_1f.f
!*  DATE                       : Jan. 19, 2009
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 361232
!*
!*  DESCRIPTION:
!*
!*  12.4.1 1 Actual arguments, dummy arguments, and argument association
!*
!*  - If an argument keyword appears, the actual argument is associated with
!*    the dummy argument whose name is the same as the argument keyword
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
type(base(4,:)), allocatable, target :: x,y
type(base(4,:)), pointer :: p
allocate(base(4,8) :: x,y)
x%a = [(i,i=1,8)]
x%b = [(i,i=11,18)]
y%a = [(i,i=21,28)]
y%b = [(i,i=31,38)]

call sub_1(n=y%n,x=y)
call sub_2(y=x,x=y)
p => x
call sub_3(y=y,p=p)
p => y
call sub_3(y=y,p=p)

contains
    subroutine sub_1(x,n)
        use m
        integer, intent(in) :: n
        type(base(4,n)) :: x
        print *,x%a,'-',x%b
    end subroutine

    subroutine sub_2(x,y)
        use m
        type(base(4,*)) :: x
        type(base(x%k,x%n)) :: y
        print *,x%a,'-',x%b
        print *,y%a,'-',y%b
    end subroutine

    subroutine sub_3(p,y)
        use m
        type(base(4,:)), pointer :: p
        type(base(p%k,p%n)), optional :: y
        print *,p%a,'-',p%b
        print *,y%a,'-',y%b
    end subroutine

end
