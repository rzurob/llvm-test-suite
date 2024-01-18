! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/27/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Use data pointer assignment to change
!*    the dynamic type. Unlimited polymorphic array pointer.
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
    type Base
        integer :: i = 1
    end type

    type, extends(Base) :: Child
        character(10) :: c = "abc"
    end type

    type Container
        class(*), pointer :: b(:,:) => null()
    end type
end module

program ptrAssignment004
use m
    type(Container) :: x, y
    type(Base), target :: b1(2,3)
    type(Child), target :: c1(8,5)
    integer, target :: i1(4,7)
    integer, target :: i2(6,2)

    if(.NOT. extends_type_of(y%b, x%b)) error stop 1_4
    if(.NOT. extends_type_of(x%b, y%b)) error stop 2_4
    if(same_type_as(y%b, x%b)) error stop 3_4

    x%b => b1
    y%b => c1

    if(.NOT. extends_type_of(y%b, x%b)) error stop 4_4
    if(extends_type_of(x%b, y%b)) error stop 5_4
    if(same_type_as(y%b, x%b)) error stop 6_4

    y = x

    if(.NOT. extends_type_of(y%b, x%b)) error stop 7_4
    if(.NOT. extends_type_of(x%b, y%b)) error stop 8_4
    if(.NOT. same_type_as(y%b, x%b)) error stop 9_4

    x%b => i1
    y%b => i2

    if(extends_type_of(y%b, x%b)) error stop 10_4
    if(extends_type_of(x%b, y%b)) error stop 11_4
    if(.NOT. same_type_as(y%b, x%b)) error stop 12_4
end
