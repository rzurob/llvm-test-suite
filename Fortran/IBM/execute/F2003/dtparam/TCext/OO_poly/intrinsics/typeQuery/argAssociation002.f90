! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/argAssociation002.f
! opt variations: -qck -qnok -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/25/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Arguments of the intrinsics are dummy
!*    arguments of another procedure. Dummy arguments are poly,
!*    non-pointer, and non-allocatable.
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child(n1)    ! (4,10)
        integer, len  :: n1
        character(n1) :: c
    end type

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(AbstractParent(4)) :: arg1
        class(AbstractParent(4)) :: arg2
        class(Base(4)) :: arg3
        class(Base(4)) :: arg4

        if(extends_type_of(arg1, arg2)) error stop 1_4
        if(.NOT. extends_type_of(arg2, arg1)) error stop 2_4
        if(.NOT. extends_type_of(arg2, arg3)) error stop 3_4
        if(extends_type_of(arg3, arg2)) error stop 4_4
        if(extends_type_of(arg3, arg4)) error stop 5_4
        if(.NOT. extends_type_of(arg4, arg3)) error stop 6_4

        if(same_type_as(arg1, arg2)) error stop 7_4
        if(.NOT. same_type_as(arg1, arg3)) error stop 8_4
        if(same_type_as(arg1, arg4)) error stop 9_4
        if(same_type_as(arg2, arg3)) error stop 10_4
        if(.NOT. same_type_as(arg2, arg4)) error stop 11_4
        if(same_type_as(arg3, arg4)) error stop 12_4
    end subroutine
end module

program argAssociation002
use m
    type(Base(4)) :: b1
    type(Child(4,10)) :: c1
    class(AbstractParent(4)), allocatable :: ap1
    class(Base(4)), pointer :: b2 => null()

    allocate(Base(4)::ap1)
    allocate(Child(4,10)::b2)
    call sub1(ap1, b2, b1, c1)
end
