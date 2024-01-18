! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/29/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is non-pointer, non-allocatable, unlimited poly, and is scalar.
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
        integer n
    end type

    contains

    subroutine sub1(arg1, arg2, arg3, arg4, arg5)
        class(*) :: arg1
        class(*) :: arg2
        class(*) :: arg3
        class(*) :: arg4
        class(*) :: arg5

        select type (name1=>transfer(arg2, arg1))
            type is (Base)
                print *, name1
            class default
                error stop 1_4
        end select

        associate(name1=>transfer(arg3, Child(1,2)))
            print *, name1
            if(.NOT. same_type_as(name1, Child(1,2))) error stop 2_4
        end associate

        associate(name1=>transfer(arg4, (/Child(1,2)/)))
            print *, name1
            if(.NOT. same_type_as(name1, Child(1,2))) error stop 3_4
        end associate

        select type (name1=>transfer(arg5, arg3, 2))
            type is (Child)
                print *, name1
            class default
                error stop 4_4
        end select

        select type (name1=>transfer(arg5, arg1, 3))
            type is (Base)
                print *, name1
            class default
                error stop 5_4
        end select
    end subroutine
end module

program argAssociation005
use m
    type(Base) :: b1
    type(Child) :: c1
    class(Base), pointer :: b2
    class(Child), allocatable :: c2
    class(*), allocatable :: u1

    b1 = Base(10)
    c1 = Child(7, 8)
    allocate(b2, SOURCE=Child(3,4))
    allocate(c2, SOURCE=Child(j=5,i=6))
    allocate(u1, SOURCE=Base1(11,Base(12),13,14))

    call sub1(b1, c1, b2, c2, u1)
end
