! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/20/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    MOLD: is unlimited polymorphic and is a disassociated pointer or
!*          an unallocated allocatable.
!*    A   : is specified using structure constructor with extensible
!*          non abstract type.
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

program structureConstructor001
use m
    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2

    if(.NOT. extends_type_of(Base(1), mold1)) error stop 1_4
    if(.NOT. extends_type_of(Base(1), mold2)) error stop 2_4
    if(same_type_as(Base(1), mold1)) error stop 3_4
    if(same_type_as(Base(1), mold2)) error stop 4_4

    allocate(Child::mold1)
    if(extends_type_of(Base(1), mold1)) error stop 5_4
    if(same_type_as(Base(1), mold1)) error stop 6_4
    deallocate(mold1)

    allocate(Base::mold2)
    if(.NOT. extends_type_of(Base(1), mold2)) error stop 7_4
    if(.NOT. same_type_as(Base(1), mold2)) error stop 8_4
    deallocate(mold2)
end
