! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration001.f
! opt variations: -qck -qnok -ql

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/20/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    MOLD: unlimited polymorphic and is a disassociated pointer or an
!*          unallocated allocatable.
!*    A   : non polymorphic and declared type is extensible.
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n1)    ! (4,10)
        integer, len  :: n1
        character(n1) :: c
    end type

    type ZeroBase(k2)    ! (4)
        integer, kind :: k2
    end type

    type, extends(ZeroBase) :: ZeroChild    ! (4)
    end type
end module

program typeDeclaration001
use m
    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2
    type(Child(4,10)) :: c1
    type(ZeroChild(4)) :: z1

    !----- non zero-size derived type

    if(.NOT. extends_type_of(c1, mold1)) error stop 1_4
    if(.NOT. extends_type_of(c1, mold2)) error stop 2_4

    if(same_type_as(c1, mold1)) error stop 3_4
    if(same_type_as(c1, mold2)) error stop 4_4

    allocate(Base(4)::mold1)
    if(.NOT. extends_type_of(c1, mold1)) error stop 5_4
    if(same_type_as(c1, mold1)) error stop 6_4

    deallocate(mold1)
    allocate(Child(4,10)::mold1)
    if(.NOT. extends_type_of(c1, mold1)) error stop 7_4
    if(.NOT. same_type_as(c1, mold1)) error stop 8_4

    !----- zero-size derived type

    deallocate(mold1)
    if(.NOT. extends_type_of(z1, mold1)) error stop 9_4
    if(.NOT. extends_type_of(z1, mold2)) error stop 10_4

    if(same_type_as(z1, mold1)) error stop 11_4
    if(same_type_as(z1, mold2)) error stop 12_4

    allocate(ZeroBase(4)::mold1)
    if(.NOT. extends_type_of(z1, mold1)) error stop 13_4
    if(same_type_as(z1, mold1)) error stop 14_4

    deallocate(mold1)
    allocate(ZeroChild(4)::mold1)
    if(.NOT. extends_type_of(z1, mold1)) error stop 15_4
    if(.NOT. same_type_as(z1, mold1)) error stop 16_4
end
