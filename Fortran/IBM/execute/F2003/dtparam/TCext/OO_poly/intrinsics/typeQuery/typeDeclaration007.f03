! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration007.f
! opt variations: -qck -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/26/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    MOLD: non polymorphic and declared type is extensible.
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
end module

program typeDeclaration007
use m
    type(Base(20,4)) :: arg1
    type(Child(20,4,4,10)), target :: arg2
    type(Base(:,4)), pointer :: arg3 => null()
    type(Child(:,4,4,:)), allocatable :: arg4
    type(Base(20,4)), parameter :: arg5 = Base(20,4)(1)

    type(Child(20,4,4,10)), target :: mold1
    type(Base(:,4)), pointer :: mold2 => null()
    type(Child(:,4,4,:)), allocatable :: mold3
    type(Child(20,4,4,10)), parameter :: mold4 = Child(20,4,4,10)(1, "abc")
    type(Base(20,4)) :: mold5

    if(extends_type_of(arg1, mold1)) error stop 1_4
    if(same_type_as(arg1, mold1)) error stop 2_4
    if(.NOT. extends_type_of(arg2, mold2)) error stop 3_4
    if(same_type_as(arg2, mold2)) error stop 4_4
    if(extends_type_of(arg3, mold3)) error stop 5_4
    if(same_type_as(arg3, mold3)) error stop 6_4
    if(.NOT. extends_type_of(arg4, mold4)) error stop 7_4
    if(.NOT. same_type_as(arg4, mold4)) error stop 8_4
    if(.NOT. extends_type_of(arg5, mold5)) error stop 9_4
    if(.NOT. same_type_as(arg5, mold5)) error stop 10_4
end
