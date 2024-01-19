! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qnodeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/argAssociation004.f
! opt variations: -qnock -qnok -ql -qdeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/25/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Arguments of the intrinsics are dummy
!*    arguments of another procedure. Dummy arguments are pointer or
!*    allocatable, non-poly or poly or unlimited poly.
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

    type, extends(Base) :: Child(k2,n1)    ! (4,1,10)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: c
    end type

    contains

    subroutine sub1(arg1, arg2, arg3, arg4, arg5, arg6)
        type(Base(4)), pointer :: arg1
        type(Child(4,1,*)), allocatable :: arg2
        class(Base(4)), pointer :: arg3
        class(Child(4,1,*)), allocatable :: arg4
        class(AbstractParent(4)), pointer :: arg5
        class(*), allocatable :: arg6

        if(extends_type_of(arg1, arg2)) error stop 1_4
        if(.NOT. extends_type_of(arg2, arg1)) error stop 2_4
        if(.NOT. extends_type_of(arg2, arg3)) error stop 3_4
        if(.NOT. extends_type_of(arg3, arg2)) error stop 4_4
        if(.NOT. extends_type_of(arg3, arg4)) error stop 5_4
        if(.NOT. extends_type_of(arg4, arg3)) error stop 6_4
        if(.NOT. extends_type_of(arg4, arg5)) error stop 7_4
        if(extends_type_of(arg5, arg4)) error stop 8_4
        if(extends_type_of(arg5, arg6)) error stop 9_4
        if(.NOT. extends_type_of(arg6, arg5)) error stop 10_4

        if(same_type_as(arg1, arg2)) error stop 11_4
        if(.NOT. same_type_as(arg2, arg3)) error stop 12_4
        if(.NOT. same_type_as(arg3, arg4)) error stop 13_4
        if(same_type_as(arg4, arg5)) error stop 14_4
        if(same_type_as(arg5, arg6)) error stop 15_4
    end subroutine
end module

program argAssociation004
use m
    type(Base(4)), pointer :: b1 => null()
    type(Child(4,1,10)), allocatable :: c1
    class(Base(4)), pointer :: b2 => null()
    class(Child(4,1,10)), allocatable :: c2
    class(AbstractParent(4)), pointer :: ap1
    class(*), allocatable :: u1

    allocate(b1, c1, c2)
    allocate(Child(4,1,10)::b2)
    allocate(Base(4)::ap1)
    allocate(Child(4,1,10)::u1)
    call sub1(b1, c1, b2, c2, ap1, u1)
end
