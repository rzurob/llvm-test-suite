! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/26/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    MOLD: unlimited polymorphic but not disassociated pointer or
!*          unallocated allocatable. Dynamic type is extensible.
!*    A   : unlimited polymorphic but not disassociated pointer or
!*          unallocated allocatable. Dynamic type is extensible.
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
        character(10) :: c
    end type
end module

program typeDeclaration018
use m
    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2

    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2

    if(.NOT. extends_type_of(arg1, mold1)) error stop 1_4
    if(.NOT. extends_type_of(arg1, mold2)) error stop 2_4
    if(.NOT. extends_type_of(arg2, mold1)) error stop 3_4
    if(.NOT. extends_type_of(arg2, mold2)) error stop 4_4

    if(same_type_as(arg1, mold1)) error stop 5_4
    if(same_type_as(arg1, mold2)) error stop 6_4
    if(same_type_as(arg2, mold1)) error stop 7_4
    if(same_type_as(arg2, mold2)) error stop 8_4

    allocate(Child::arg1)
    allocate(Base::arg2)
    allocate(Child::mold1)
    allocate(Base::mold2)

    if(.NOT. extends_type_of(arg1, mold1)) error stop 9_4
    if(.NOT. extends_type_of(arg1, mold2)) error stop 10_4
    if(extends_type_of(arg2, mold1)) error stop 11_4
    if(.NOT. extends_type_of(arg2, mold2)) error stop 12_4

    if(.NOT. same_type_as(arg1, mold1)) error stop 13_4
    if(same_type_as(arg1, mold2)) error stop 14_4
    if(same_type_as(arg2, mold1)) error stop 15_4
    if(.NOT. same_type_as(arg2, mold2)) error stop 16_4
end