! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/OO_poly/intrinsics/typeQuery/structureConstructor003.f
! opt variations: -qnock -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/20/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    MOLD: is specified using structure constructor with extensible
!*          non abstract type.
!*    A   : non polymorphic with extensible type.
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

program structureConstructor003
use m
    type(Base(20,4)) :: arg1
    type(Child(20,4,1,10)), target :: arg2
    type(Base(:,4)), pointer :: arg3 => null()
    type(Child(:,4,1,:)), allocatable :: arg4
    type(Base(20,4)), parameter :: arg5 = Base(20,4)(1)

    if(extends_type_of(arg1, Child(20,4,1,10)(1, "abc"))) error stop 1_4
    if(.NOT. extends_type_of(arg2, Child(20,4,1,10)(1, "abc"))) error stop 2_4
    if(extends_type_of(arg3, Child(20,4,1,10)(1, "abc"))) error stop 3_4
    if(.NOT. extends_type_of(arg4, Child(20,4,1,10)(1, "abc"))) error stop 4_4
    if(extends_type_of(arg5, Child(20,4,1,10)(1, "abc"))) error stop 5_4

    if(same_type_as(arg1, Child(20,4,1,10)(1, "abc"))) error stop 6_4
    if(.NOT. same_type_as(arg2, Child(20,4,1,10)(1, "abc"))) error stop 7_4
    if(same_type_as(arg3, Child(20,4,1,10)(1, "abc"))) error stop 8_4
    if(.NOT. same_type_as(arg4, Child(20,4,1,10)(1, "abc"))) error stop 9_4
    if(same_type_as(arg5, Child(20,4,1,10)(1, "abc"))) error stop 10_4
end
