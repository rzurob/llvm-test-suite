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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type

    contains

    subroutine sub1(arg1, arg2, arg3, arg4, arg5, arg6)
        type(Base), pointer :: arg1
        type(Child), allocatable :: arg2
        class(Base), pointer :: arg3
        class(Child), allocatable :: arg4
        class(AbstractParent), pointer :: arg5
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
    type(Base), pointer :: b1 => null()
    type(Child), allocatable :: c1
    class(Base), pointer :: b2 => null()
    class(Child), allocatable :: c2
    class(AbstractParent), pointer :: ap1
    class(*), allocatable :: u1

    allocate(b1, c1, c2)
    allocate(Child::b2)
    allocate(Base::ap1)
    allocate(Child::u1)
    call sub1(b1, c1, b2, c2, ap1, u1)
end
