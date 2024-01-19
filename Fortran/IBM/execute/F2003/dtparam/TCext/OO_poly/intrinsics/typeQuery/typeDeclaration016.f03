! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration016.f
! opt variations: -qnock -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/26/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    MOLD: unlimited polymorphic but not disassociated pointer or
!*          unallocated allocatable. Dynamic type is extensible.
!*    A   : polymorphic but not unlimited polymorphic. Declared type is
!*          extensible and can be abstract or non-abstract.
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

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child(k2,n2)    ! (4,20,1,10)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: c
    end type
end module

program typeDeclaration016
use m
    class(Base(4,:)), pointer :: arg1 => null()
    class(AbstractParent(4,:)), allocatable :: arg2

    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2

    allocate(Child(4,20,1,10)::mold1)
    allocate(Base(4,20)::mold2)

    if(extends_type_of(arg1, mold1)) error stop 1_4
    if(.NOT. extends_type_of(arg1, mold2)) error stop 2_4
    if(extends_type_of(arg2, mold1)) error stop 3_4
    if(extends_type_of(arg2, mold2)) error stop 4_4

    if(same_type_as(arg1, mold1)) error stop 5_4
    if(.NOT. same_type_as(arg1, mold2)) error stop 6_4
    if(same_type_as(arg2, mold1)) error stop 7_4
    if(same_type_as(arg2, mold2)) error stop 8_4

    allocate(Base(4,20)::arg1)
    allocate(Child(4,20,1,10)::arg2)

    if(extends_type_of(arg1, mold1)) error stop 9_4
    if(.NOT. extends_type_of(arg1, mold2)) error stop 10_4
    if(.NOT. extends_type_of(arg2, mold1)) error stop 11_4
    if(.NOT. extends_type_of(arg2, mold2)) error stop 12_4

    if(same_type_as(arg1, mold1)) error stop 13_4
    if(.NOT. same_type_as(arg1, mold2)) error stop 14_4
    if(.NOT. same_type_as(arg2, mold1)) error stop 15_4
    if(same_type_as(arg2, mold2)) error stop 16_4
end
