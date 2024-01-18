! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration010.f
! opt variations: -qnol -qreuse=self

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/26/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    MOLD: non polymorphic and declared type is extensible.
!*    A   : unlimited polymorphic, but not disassociated pointer or
!*          unallocated allocatable. Dynamic type is non extensible.
!*    Note: If A is unlimited polymorphic, the dynamic type of A
!*          is allowed to be non extensible.
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
    type SequenceBase(n1,k1,k2)    ! (20,4,4)
        integer, kind :: k1,k2
        integer, len  :: n1
        sequence
        integer(k1)      i
        integer(k2)      j
    end type

    type Base(n2,k3)    ! (20,4)
        integer, kind :: k3
        integer, len  :: n2
        integer(k3)      i
    end type
end module

program typeDeclaration010
use m
    type(Base(20,4)) :: mold1
    class(*), pointer :: arg1 => null()

    allocate(integer::arg1)
    if(extends_type_of(arg1, mold1)) error stop 1_4
    if(same_type_as(arg1, mold1)) error stop 2_4

    deallocate(arg1)
    allocate(SequenceBase(20,4,4)::arg1)
    if(extends_type_of(arg1, mold1)) error stop 3_4
    if(same_type_as(arg1, mold1)) error stop 4_4
end
