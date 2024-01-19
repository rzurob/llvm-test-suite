! GB DTP extension using:
! ftcx_dtp -qck -ql /tstdev/OO_poly/intrinsics/typeQuery/structureConstructor011.f
! opt variations: -qnock -qnol

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/27/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    A   : is specified using structure constructor with extensible
!*          non abstract type.
!*    MOLD: is specified using structure constructor with extensible
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

program structureConstructor011
use m
    if(.NOT. extends_type_of(Base(20,4)(1), Base(20,4)(2))) error stop 1_4
    if(extends_type_of(Base(20,4)(1), Child(20,4,1,10)(1,"abc"))) error stop 2_4
    if(.NOT. extends_type_of(Child(20,4,1,10)(1,"abc"), Base(20,4)(1))) error stop 3_4
    if(.NOT. extends_type_of(Child(20,4,1,10)(1,"abc"), Child(20,4,1,10)(3, "def"))) error stop 4_4

    if(.NOT. same_type_as(Base(20,4)(1), Base(20,4)(2))) error stop 5_4
    if(same_type_as(Base(20,4)(1), Child(20,4,1,10)(1,"abc"))) error stop 6_4
    if(same_type_as(Child(20,4,1,10)(1,"abc"), Base(20,4)(1))) error stop 7_4
    if(.NOT. same_type_as(Child(20,4,1,10)(1,"abc"), Child(20,4,1,10)(3, "def"))) error stop 8_4
end
