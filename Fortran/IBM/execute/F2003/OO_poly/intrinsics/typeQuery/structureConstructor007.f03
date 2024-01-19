! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/27/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                :
!*    A   : is specified using structure constructor with extensible
!*          non abstract type.
!*    MOLD: non polymorphic
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

program structureConstructor007
use m
    type(Base) :: mold1
    type(Child), target :: mold2
    type(Base), pointer :: mold3 => null()
    type(Child), allocatable :: mold4
    type(Base), parameter :: mold5 = Base(1)

    if(.NOT. extends_type_of(Base(1), mold1)) error stop 1_4
    if(extends_type_of(Base(1), mold2)) error stop 2_4
    if(.NOT. extends_type_of(Child(1, "abc"), mold3)) error stop 3_4
    if(extends_type_of(Base(1), mold4)) error stop 4_4
    if(.NOT. extends_type_of(Base(1), mold5)) error stop 5_4

    if(.NOT. same_type_as(Base(1), mold1)) error stop 6_4
    if(same_type_as(Base(1), mold2)) error stop 7_4
    if(same_type_as(Child(1, "abc"), mold3)) error stop 8_4
    if(.NOT. same_type_as(Child(1, "abc"), mold4)) error stop 9_4
    if(.NOT. same_type_as(Base(1), mold5)) error stop 10_4
end
