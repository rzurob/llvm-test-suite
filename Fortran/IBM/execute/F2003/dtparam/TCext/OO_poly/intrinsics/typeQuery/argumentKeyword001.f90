! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/typeQuery/argumentKeyword001.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/25/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Actual arguments are specified using
!*    argument keywords.
!*
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
    type, abstract :: AbstractParent(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      a
    end type

    type, extends(AbstractParent) :: Base(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(k3,n3)    ! (20,4,20,4,4,10)
        integer, kind :: k3
        integer, len  :: n3
        character(n3) :: c
    end type
end module

program argumentKeyword001
use m
    type(Child(20,4,20,4,4,10)) :: a
    type(Child(20,4,20,4,4,10)) :: a1
    class(Base(:,4,:,4)), allocatable :: mold
    class(Base(:,4,:,4)), allocatable :: b

    if(.NOT. extends_type_of(A=a, MOLD=mold)) error stop 1_4
    if(.NOT. extends_type_of(MOLD=mold, A=a)) error stop 2_4
    if(.NOT. extends_type_of(a, MOLD=mold)) error stop 3_4
    if(extends_type_of(MOLD=a, A=mold)) error stop 4_4
    if(.NOT. extends_type_of(A=a1, MOLD=b)) error stop 5_4

    allocate(Child(20,4,20,4,4,10)::b)

    if(.NOT. same_type_as(A=a, B=b)) error stop 6_4
    if(.NOT. same_type_as(B=b, A=a)) error stop 7_4
    if(.NOT. same_type_as(a, B=b)) error stop 8_4
    if(.NOT. same_type_as(B=a, A=b)) error stop 9_4
    if(same_type_as(A=a1, B=mold)) error stop 10_4
end
