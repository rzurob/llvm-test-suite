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
    type, abstract :: AbstractParent
        integer a
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type
end module

program argumentKeyword001
use m
    type(Child) :: a
    type(Child) :: a1
    class(Base), allocatable :: mold
    class(Base), allocatable :: b

    if(.NOT. extends_type_of(A=a, MOLD=mold)) error stop 1_4
    if(.NOT. extends_type_of(MOLD=mold, A=a)) error stop 2_4
    if(.NOT. extends_type_of(a, MOLD=mold)) error stop 3_4
    if(extends_type_of(MOLD=a, A=mold)) error stop 4_4
    if(.NOT. extends_type_of(A=a1, MOLD=b)) error stop 5_4

    allocate(Child::b)

    if(.NOT. same_type_as(A=a, B=b)) error stop 6_4
    if(.NOT. same_type_as(B=b, A=a)) error stop 7_4
    if(.NOT. same_type_as(a, B=b)) error stop 8_4
    if(.NOT. same_type_as(B=a, A=b)) error stop 9_4
    if(same_type_as(A=a1, B=mold)) error stop 10_4
end
