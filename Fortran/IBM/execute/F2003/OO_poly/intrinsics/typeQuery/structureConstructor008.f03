! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/20/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    A   : is specified using structure constructor with extensible
!*          non abstract type.
!*    MOLD: polymorphic but not unlimited polymorphic, declared type
!*          can be abstract or non-abstract.
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

program structureConstructor008
use m
    class(Base), pointer :: mold1 => null()
    class(AbstractParent), allocatable :: mold2

    if(.NOT. extends_type_of(Base(1), mold1)) error stop 1_4
    if(.NOT. extends_type_of(Child(1, "abc"), mold2)) error stop 2_4
    if(.NOT. same_type_as(Base(1), mold1)) error stop 3_4
    if(same_type_as(Child(1, "abc"), mold2)) error stop 4_4

    allocate(Child::mold1)
    allocate(Base::mold2)

    if(extends_type_of(Base(1), mold1)) error stop 5_4
    if(.NOT. extends_type_of(Child(1, "abc"), mold2)) error stop 6_4
    if(.NOT. same_type_as(Child(1, "abc"), mold1)) error stop 7_4
    if(.NOT. same_type_as(Base(1), mold2)) error stop 8_4
end