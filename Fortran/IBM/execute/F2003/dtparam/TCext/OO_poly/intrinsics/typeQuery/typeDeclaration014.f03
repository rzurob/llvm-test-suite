! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration014.f
! opt variations: -qnock -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/26/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    MOLD: unlimited polymorphic but not disassociated pointer or
!*          unallocated allocatable. Dynamic type is extensible.
!*    A   : Non polymorphic and declared type is extensible.
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

program typeDeclaration014
use m
    type(Base(20,4)) :: arg1
    type(Child(20,4,1,10)), target :: arg2
    type(Base(:,4)), pointer :: arg3 => null()
    type(Child(:,4,1,:)), allocatable :: arg4
    type(Base(20,4)), parameter :: arg5 = Base(20,4)(1)

    class(*), pointer :: mold1 => null()
    class(*), allocatable :: mold2

    if(.NOT. extends_type_of(arg1, mold1)) error stop 1_4
    if(.NOT. extends_type_of(arg1, mold2)) error stop 2_4
    if(.NOT. extends_type_of(arg2, mold1)) error stop 3_4
    if(.NOT. extends_type_of(arg2, mold2)) error stop 4_4
    if(.NOT. extends_type_of(arg3, mold1)) error stop 5_4
    if(.NOT. extends_type_of(arg3, mold2)) error stop 6_4
    if(.NOT. extends_type_of(arg4, mold1)) error stop 7_4
    if(.NOT. extends_type_of(arg4, mold2)) error stop 8_4
    if(.NOT. extends_type_of(arg5, mold1)) error stop 9_4
    if(.NOT. extends_type_of(arg5, mold2)) error stop 10_4

    if(same_type_as(arg1, mold1)) error stop 11_4
    if(same_type_as(arg1, mold2)) error stop 12_4
    if(same_type_as(arg2, mold1)) error stop 13_4
    if(same_type_as(arg2, mold2)) error stop 14_4
    if(same_type_as(arg3, mold1)) error stop 15_4
    if(same_type_as(arg3, mold2)) error stop 16_4
    if(same_type_as(arg4, mold1)) error stop 17_4
    if(same_type_as(arg4, mold2)) error stop 18_4
    if(same_type_as(arg5, mold1)) error stop 19_4
    if(same_type_as(arg5, mold2)) error stop 20_4

    allocate(Child(20,4,1,10)::mold1)
    allocate(Base(20,4)::mold2)

    if(extends_type_of(arg1, mold1)) error stop 21_4
    if(.NOT. extends_type_of(arg1, mold2)) error stop 22_4
    if(.NOT. extends_type_of(arg2, mold1)) error stop 23_4
    if(.NOT. extends_type_of(arg2, mold2)) error stop 24_4
    if(extends_type_of(arg3, mold1)) error stop 25_4
    if(.NOT. extends_type_of(arg3, mold2)) error stop 26_4
    if(.NOT. extends_type_of(arg4, mold1)) error stop 27_4
    if(.NOT. extends_type_of(arg4, mold2)) error stop 28_4
    if(extends_type_of(arg5, mold1)) error stop 29_4
    if(.NOT. extends_type_of(arg5, mold2)) error stop 30_4

    if(same_type_as(arg1, mold1)) error stop 31_4
    if(.NOT. same_type_as(arg1, mold2)) error stop 32_4
    if(.NOT. same_type_as(arg2, mold1)) error stop 33_4
    if(same_type_as(arg2, mold2)) error stop 34_4
    if(same_type_as(arg3, mold1)) error stop 35_4
    if(.NOT. same_type_as(arg3, mold2)) error stop 36_4
    if(.NOT. same_type_as(arg4, mold1)) error stop 37_4
    if(same_type_as(arg4, mold2)) error stop 38_4
    if(same_type_as(arg5, mold1)) error stop 39_4
    if(.NOT. same_type_as(arg5, mold2)) error stop 40_4
end
