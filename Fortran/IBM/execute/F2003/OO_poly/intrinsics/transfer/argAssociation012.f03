! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/29/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is a pointer or allocatable, unlimited poly, and is array.
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

program argAssociation012
use m
    class(*), pointer :: b(:)
    class(*), allocatable :: b1(:,:)

    allocate(b(10), SOURCE=(/(Child(i,i+1),i=1,10)/))
    allocate(b1(2,3), SOURCE=reshape((/(Child1(i, Base(i+1), i+2, &
     i+3),i=3,8)/), (/2,3/), (/child1(1,Base(1),1,0)/), (/2,1/)))

    call sub1(b, b1)

    contains

    subroutine sub1(arg1, arg2)
        class(*), pointer :: arg1(:)
        class(*), allocatable :: arg2(:,:)

        select type(name1=>transfer(arg1, arg2))
            type is (Child1)
                print *, name1
            class default
                error stop 1_4
        end select

        select type(name1=>transfer(arg2, arg1))
            type is (Child)
                print *, name1
            class default
                error stop 2_4
        end select
    end subroutine
end