! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/29/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is non-pointer, non-allocatable, non-poly, and is array.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    type Base1
        integer j
        type(Base) :: k
        integer m
    end type

    type, extends(Base1) :: Child1
        integer n
    end type
end module

program argAssociation002
use m
    type(Base) :: b(10)
    type(Base1) :: b1(2,3)
    class(Base), pointer :: c(:)
    class(Base1), allocatable :: c1(:,:)

    b = (/ (Base(i),i=1,10) /)
    b1 = reshape((/(Base1(i, Base(i+1), i+2),i=5,15,2)/),(/2,3/))
    allocate(c(6), SOURCE=(/(Child(i,i+1),i=2,7)/))
    allocate(c1(2,2), SOURCE=reshape((/(Child1(m=i+2,n=i+3, &
     k=Base(i+1),j=i),i=12,15)/), (/2,2/)))

    call sub1(b, b1, c, c1)

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        type(Base) :: arg1(10)
        type(Base1) :: arg2(:,:)
        type(Base) :: arg3(:)
        type(Base1) :: arg4(2,2)

        print *, transfer(arg3, arg1)
        print *, transfer(arg2, Child(1,2), 5)
        print *, transfer(arg4, (/Child1(1,Base(1),1,1)/))
    end subroutine
end
