! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration003.f
! opt variations: -qck -qnok -qnol -qreuse=self

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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2,n2)    ! (20,4,4,10)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: c
    end type

    type SequenceBase(n3,k3,k4)    ! (20,4,4)
        integer, kind :: k3,k4
        integer, len  :: n3
        sequence
        integer(k3)      i
        integer(k4)      j
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

    allocate(SequenceBase(20,4,4)::u1)
    if(.NOT. extends_type_of(u1, mold2)) error stop 9_4
    if(same_type_as(u1, mold2)) error stop 10_4
    deallocate(u1)
    if(.NOT. extends_type_of(u1, mold2)) error stop 11_4
    if(same_type_as(u1, mold2)) error stop 12_4

    !--------- dynamic type of A is extensible

    allocate(Base(20,4)::u1)
    allocate(Child(20,4,4,10)::mold1)
    if(extends_type_of(u1, mold1)) error stop 13_4
    if(same_type_as(u1, mold1)) error stop 14_4
    deallocate(u1, mold1)
    if(.NOT. extends_type_of(u1, mold1)) error stop 15_4
    if(same_type_as(u1, mold1)) error stop 16_4
end
