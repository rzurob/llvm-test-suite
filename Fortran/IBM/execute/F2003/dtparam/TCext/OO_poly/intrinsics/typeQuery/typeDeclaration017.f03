! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration017.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=self -qreuse=base

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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(k3,n3)    ! (4,20,20,4,4,10)
        integer, kind :: k3
        integer, len  :: n3
        character(n3) :: c
    end type

    type SequenceBase(n4,k4,k5)    ! (20,4,4)
        integer, kind :: k4,k5
        integer, len  :: n4
        sequence
        integer(k4)      i
        integer(k5)      j
    end type
end module

program typeDeclaration017
use m
    class(Base(4,:,:,4)), pointer :: arg1 => null()
    class(AbstractParent(4,:)), allocatable :: arg2

    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2

    allocate(integer::mold1)
    allocate(SequenceBase(20,4,4)::mold2)

    if(extends_type_of(arg1, mold1)) error stop 1_4
    if(extends_type_of(arg1, mold2)) error stop 2_4
    if(extends_type_of(arg2, mold1)) error stop 3_4
    if(extends_type_of(arg2, mold2)) error stop 4_4

    if(same_type_as(arg1, mold1)) error stop 5_4
    if(same_type_as(arg1, mold2)) error stop 6_4
    if(same_type_as(arg2, mold1)) error stop 7_4
    if(same_type_as(arg2, mold2)) error stop 8_4

    allocate(Base(4,20,20,4)::arg1)
    allocate(Child(4,20,20,4,4,10)::arg2)

    if(extends_type_of(arg1, mold1)) error stop 9_4
    if(extends_type_of(arg1, mold2)) error stop 10_4
    if(extends_type_of(arg2, mold1)) error stop 11_4
    if(extends_type_of(arg2, mold2)) error stop 12_4

    if(same_type_as(arg1, mold1)) error stop 13_4
    if(same_type_as(arg1, mold2)) error stop 14_4
    if(same_type_as(arg2, mold1)) error stop 15_4
    if(same_type_as(arg2, mold2)) error stop 16_4
end
