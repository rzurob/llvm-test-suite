! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/25/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    MOLD: unlimited polymorphic and is a disassociated pointer or an
!*          unallocated allocatable.
!*    A   : unlimited polymorphic. Dynamic type can be either extensible
!*          or non-extensible, including intrinsics and sequence types,
!*          but cannot be abstract.
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

    type SequenceBase
        sequence
        integer i
        integer j
    end type
end module

program typeDeclaration003
use m
    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2
    class(*), pointer :: u1 => null()

    !--------- both MOLD and A are unlimited polymorphic
    !--------- and either disassociated pointer or unallocated
    !--------- allocatable
    if(.NOT. extends_type_of(u1, mold1)) error stop 1_4
    if(.NOT. extends_type_of(u1, mold2)) error stop 2_4
    if(same_type_as(u1, mold1)) error stop 3_4
    if(same_type_as(u1, mold2)) error stop 4_4

    !--------- dynamic type of A is intrinsic

    allocate(integer::u1)
    if(.NOT. extends_type_of(u1, mold1)) error stop 5_4
    if(same_type_as(u1, mold1)) error stop 6_4
    deallocate(u1)
    if(.NOT. extends_type_of(u1, mold1)) error stop 7_4
    if(same_type_as(u1, mold1)) error stop 8_4

    !--------- dynamic type of A is sequence type

    allocate(SequenceBase::u1)
    if(.NOT. extends_type_of(u1, mold2)) error stop 9_4
    if(same_type_as(u1, mold2)) error stop 10_4
    deallocate(u1)
    if(.NOT. extends_type_of(u1, mold2)) error stop 11_4
    if(same_type_as(u1, mold2)) error stop 12_4

    !--------- dynamic type of A is extensible

    allocate(Base::u1)
    allocate(Child::mold1)
    if(extends_type_of(u1, mold1)) error stop 13_4
    if(same_type_as(u1, mold1)) error stop 14_4
    deallocate(u1, mold1)
    if(.NOT. extends_type_of(u1, mold1)) error stop 15_4
    if(same_type_as(u1, mold1)) error stop 16_4
end
