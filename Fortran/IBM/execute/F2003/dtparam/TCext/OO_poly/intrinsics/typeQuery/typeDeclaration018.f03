! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration018.f
! opt variations: -qck -qnol

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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n2)    ! (20,4,10)
        integer, len  :: n2
        character(n2) :: c
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

    allocate(Child(20,4,10)::arg1)
    allocate(Base(20,4)::arg2)
    allocate(Child(20,4,10)::mold1)
    allocate(Base(20,4)::mold2)

    if(.NOT. extends_type_of(arg1, mold1)) error stop 9_4
    if(.NOT. extends_type_of(arg1, mold2)) error stop 10_4
    if(extends_type_of(arg2, mold1)) error stop 11_4
    if(.NOT. extends_type_of(arg2, mold2)) error stop 12_4

    if(.NOT. same_type_as(arg1, mold1)) error stop 13_4
    if(same_type_as(arg1, mold2)) error stop 14_4
    if(same_type_as(arg2, mold1)) error stop 15_4
    if(.NOT. same_type_as(arg2, mold2)) error stop 16_4
end
