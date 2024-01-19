! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/29/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is non-pointer, non-allocatable, non-poly, and is scalar.
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

program argAssociation001
use m
    type(Base) :: b
    type(Base1) :: b1
    class(Base), pointer :: c
    class(Base1), allocatable :: c1

    b = Base(10)
    b1 = Base1(7, Base(8), 9)
    allocate(c, SOURCE=Child(3,4))
    allocate(c1, SOURCE=Child1(m=14,n=15,k=Base(13),j=12))

    call sub1(b, b1, c, c1)

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        type(Base) :: arg1
        type(Base1) :: arg2
        type(Base) :: arg3
        type(Base1) :: arg4

        print *, transfer(arg2, arg1, 3)
        print *, transfer(arg4, arg3, 3)
    end subroutine
end
