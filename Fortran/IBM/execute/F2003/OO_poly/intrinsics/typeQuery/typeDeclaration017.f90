! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/26/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    MOLD: unlimited polymorphic but not disassociated pointer or
!*          unallocated allocatable. Dynamic type is not extensible.
!*    A   : polymorphic but not unlimited polymorphic. Declared type is
!*          extensible and can be abstract or non-abstract.
!*    Note: If A is unlimited polymorphic, the dynamic type of A
!*          can be non extensible.
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

    type SequenceBase
        sequence
        integer i
        integer j
    end type
end module

program typeDeclaration017
use m
    class(Base), pointer :: arg1 => null()
    class(AbstractParent), allocatable :: arg2

    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2

    allocate(integer::mold1)
    allocate(SequenceBase::mold2)

    if(extends_type_of(arg1, mold1)) error stop 1_4
    if(extends_type_of(arg1, mold2)) error stop 2_4
    if(extends_type_of(arg2, mold1)) error stop 3_4
    if(extends_type_of(arg2, mold2)) error stop 4_4

    if(same_type_as(arg1, mold1)) error stop 5_4
    if(same_type_as(arg1, mold2)) error stop 6_4
    if(same_type_as(arg2, mold1)) error stop 7_4
    if(same_type_as(arg2, mold2)) error stop 8_4

    allocate(Base::arg1)
    allocate(Child::arg2)

    if(extends_type_of(arg1, mold1)) error stop 9_4
    if(extends_type_of(arg1, mold2)) error stop 10_4
    if(extends_type_of(arg2, mold1)) error stop 11_4
    if(extends_type_of(arg2, mold2)) error stop 12_4

    if(same_type_as(arg1, mold1)) error stop 13_4
    if(same_type_as(arg1, mold2)) error stop 14_4
    if(same_type_as(arg2, mold1)) error stop 15_4
    if(same_type_as(arg2, mold2)) error stop 16_4
end
