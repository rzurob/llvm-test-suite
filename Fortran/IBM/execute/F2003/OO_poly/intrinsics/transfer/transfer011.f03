! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SIZE is absent
!*    MOLD is scalar
!*    SOURCE is scalar
!*    Physical representation of result has shorter length than that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD. The physical representation is the leading part of that of
!*  SOURCE.
!*    Poly
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
        integer k
        integer m
    end type

    type, extends(Base1) :: Child1
        integer i
        integer j
    end type
end module

program transfer011
use m
    class(Base1), allocatable :: src1
    class(Base), pointer :: m1

    allocate(src1, SOURCE=Base1(8,9))

    select type(src1)
        type is (Base1)
            print *, src1
        class default
            error stop 1_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Base)
            print *, name1
        class default
            error stop 2_4
    end select

    deallocate(src1)
    allocate(src1, SOURCE=Child1(4,5,6,7))
    allocate(m1, SOURCE=Child(3,4))

    select type(src1)
        type is (Child1)
            print *, src1
        class default
            error stop 3_4
    end select

    select type(m1)
        type is (Child)
            print *, m1
        class default
            error stop 4_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Child)
            print *, name1
        class default
            error stop 5_4
    end select
end