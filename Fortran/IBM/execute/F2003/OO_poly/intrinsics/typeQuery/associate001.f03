! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/25/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Query type inside an associate construct.
!*    Selector is scalar.
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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type
end module

program associate001
use m
    class(*), pointer :: i => null()
    class(*), pointer :: ap1 => null()
    class(AbstractParent), allocatable :: ap2

    allocate(Base::ap1)

    associate(name1=>ap1, name2=>ap2)
        if(.NOT. extends_type_of(name1, name2)) error stop 1_4
        if(.NOT. extends_type_of(name1, ap2)) error stop 2_4
        if(.NOT. extends_type_of(name1, i)) error stop 3_4
        if(.NOT. extends_type_of(name2, i)) error stop 4_4
        if(same_type_as(name1, name2)) error stop 5_4
        if(.NOT. same_type_as(name1, ap1)) error stop 6_4
        if(.NOT. same_type_as(name2, ap2)) error stop 7_4
        if(same_type_as(name1, i)) error stop 8_4
    end associate

    allocate(Child::ap2)
    allocate(integer::i)

    associate(name1=>ap1, name2=>ap2)
        if(.NOT. extends_type_of(name2, name1)) error stop 9_4
        if(.NOT. extends_type_of(name2, ap1)) error stop 10_4
        if(same_type_as(name2, name1)) error stop 11_4
        if(.NOT. same_type_as(name1, ap1)) error stop 12_4
        if(.NOT. same_type_as(name2, ap2)) error stop 13_4
    end associate
end
