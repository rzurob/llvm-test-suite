! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/25/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    MOLD: unlimited polymorphic and is a disassociated pointer or an
!*          unallocated allocatable.
!*    A   : polymorphic but not unlimited polymorphic. Declared type is
!*          extensible, and can be either abstract or non-abstract.
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
        integer a
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type

    type, abstract :: ZeroAbstractParent
    end type

    type, extends(ZeroAbstractParent) :: ZeroBase
    end type

    type, extends(Base) :: ZeroChild
    end type
end module

program typeDeclaration002
use m
    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2
    class(Base), pointer :: b1 => null()
    class(AbstractParent), pointer :: ap1 => null()
    class(ZeroBase), pointer :: z1 => null()
    class(ZeroAbstractParent), pointer :: ap2 => null()

    !===== non zero-size derived type

    !-------- non abstract type

    if(.NOT. extends_type_of(b1, mold1)) error stop 1_4
    if(.NOT. extends_type_of(b1, mold2)) error stop 2_4
    if(same_type_as(b1, mold1)) error stop 3_4
    if(same_type_as(b1, mold2)) error stop 4_4

    allocate(Base::mold1)
    if(.NOT. extends_type_of(b1, mold1)) error stop 5_4
    if(.NOT. same_type_as(b1, mold1)) error stop 6_4
    deallocate(mold1)
    allocate(Child::mold1)
    if(extends_type_of(b1, mold1)) error stop 7_4
    if(same_type_as(b1, mold1)) error stop 8_4
    deallocate(mold1)

    !-------- abstract type

    if(.NOT. extends_type_of(ap1, mold1)) error stop 9_4
    if(.NOT. extends_type_of(ap1, mold2)) error stop 10_4
    if(same_type_as(ap1, mold1)) error stop 11_4
    if(same_type_as(ap1, mold2)) error stop 12_4

    allocate(Base::mold2)
    if(extends_type_of(ap1, mold2)) error stop 13_4
    if(same_type_as(ap1, mold2)) error stop 14_4
    allocate(Base::ap1)
    if(.NOT. extends_type_of(ap1, mold2)) error stop 15_4
    if(.NOT. same_type_as(ap1, mold2)) error stop 16_4
    deallocate(mold2)

    !===== zero-size derived type

    !-------- non abstract type

    if(.NOT. extends_type_of(z1, mold1)) error stop 17_4
    if(.NOT. extends_type_of(z1, mold2)) error stop 18_4
    if(same_type_as(z1, mold1)) error stop 19_4
    if(same_type_as(z1, mold2)) error stop 20_4

    allocate(ZeroBase::mold1)
    if(.NOT. extends_type_of(z1, mold1)) error stop 21_4
    if(.NOT. same_type_as(z1, mold1)) error stop 22_4
    deallocate(mold1)
    allocate(ZeroChild::mold1)
    if(extends_type_of(z1, mold1)) error stop 23_4
    if(same_type_as(z1, mold1)) error stop 24_4
    deallocate(mold1)

    !-------- abstract type

    if(.NOT. extends_type_of(ap2, mold1)) error stop 25_4
    if(.NOT. extends_type_of(ap2, mold2)) error stop 26_4
    if(same_type_as(ap2, mold1)) error stop 27_4
    if(same_type_as(ap2, mold2)) error stop 28_4

    allocate(ZeroBase::mold2)
    if(extends_type_of(ap2, mold2)) error stop 29_4
    if(same_type_as(ap2, mold2)) error stop 30_4
    allocate(ZeroBase::ap2)
    if(.NOT. extends_type_of(ap2, mold2)) error stop 31_4
    if(.NOT. same_type_as(ap2, mold2)) error stop 32_4
    deallocate(mold2)
end
