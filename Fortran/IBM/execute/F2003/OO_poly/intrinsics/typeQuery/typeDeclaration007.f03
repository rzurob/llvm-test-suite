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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type
end module

program typeDeclaration007
use m
    type(Base) :: arg1
    type(Child), target :: arg2
    type(Base), pointer :: arg3 => null()
    type(Child), allocatable :: arg4
    type(Base), parameter :: arg5 = Base(1)

    type(Child), target :: mold1
    type(Base), pointer :: mold2 => null()
    type(Child), allocatable :: mold3
    type(Child), parameter :: mold4 = Child(1, "abc")
    type(Base) :: mold5

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
