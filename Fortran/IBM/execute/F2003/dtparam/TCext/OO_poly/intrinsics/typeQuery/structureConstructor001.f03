! GB DTP extension using:
! ftcx_dtp -qck -ql /tstdev/OO_poly/intrinsics/typeQuery/structureConstructor001.f
! opt variations: -qnock -qnol

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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2,n2)    ! (20,4,1,10)
        integer, kind             :: k2
        integer, len              :: n2
        character(kind=k2,len=n2) :: c
    end type
end module

program structureConstructor001
use m
    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2

    if(.NOT. extends_type_of(Base(20,4)(1), mold1)) error stop 1_4
    if(.NOT. extends_type_of(Base(20,4)(1), mold2)) error stop 2_4
    if(same_type_as(Base(20,4)(1), mold1)) error stop 3_4
    if(same_type_as(Base(20,4)(1), mold2)) error stop 4_4

    allocate(Child(20,4,1,10)::mold1)
    if(extends_type_of(Base(20,4)(1), mold1)) error stop 5_4
    if(same_type_as(Base(20,4)(1), mold1)) error stop 6_4
    deallocate(mold1)

    allocate(Base(20,4)::mold2)
    if(.NOT. extends_type_of(Base(20,4)(1), mold2)) error stop 7_4
    if(.NOT. same_type_as(Base(20,4)(1), mold2)) error stop 8_4
    deallocate(mold2)
end
