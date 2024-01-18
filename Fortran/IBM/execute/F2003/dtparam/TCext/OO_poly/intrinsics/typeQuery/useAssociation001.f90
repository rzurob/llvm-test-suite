! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/OO_poly/intrinsics/typeQuery/useAssociation001.f
! opt variations: -qnock -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/27/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Type renaming in use statement.
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

module n
    type Base(n3,k3)    ! (20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      i
    end type

    type, extends(Base) :: Child(k4,n4)    ! (20,4,1,10)
        integer, kind             :: k4
        integer, len              :: n4
        character(kind=k4,len=n4) :: c
    end type
end module

program useAssociation001
use m, myBase => Base, myChild => Child
use m, only : Base
use n, only : Child
    type(myChild(20,4,1,10)) :: arg1
    class(myBase(:,4)), allocatable :: mold1
    class(Base(:,4)), pointer :: mold2 => null()
    type(Base(20,4)) :: mold3
    type(Child(20,4,1,10)) :: arg2

    if(.NOT. extends_type_of(arg1, mold1)) error stop 1_4
    if(.NOT. extends_type_of(arg1, mold2)) error stop 2_4
    if(extends_type_of(arg2, mold3)) error stop 3_4

    if(same_type_as(arg1, mold1)) error stop 4_4
    if(.NOT. same_type_as(mold1, mold2)) error stop 5_4
    if(same_type_as(arg2, mold3)) error stop 6_4
end
