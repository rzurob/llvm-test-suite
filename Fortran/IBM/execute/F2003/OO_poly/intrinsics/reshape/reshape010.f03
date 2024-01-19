! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is poly
!*    Assigned data entity is non-poly
!*    PAD and ORDER are not specified
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
        integer :: i = 88
    end type

    type, extends(Base) :: Child
        integer :: j = 99
    end type
end module

program reshape010
use m
    class(Base), pointer :: b1(:) => null()
    type(Base) :: c1(3,5)
    allocate(b1(20), SOURCE = (/ (Child(i,i+100), i=1,20) /))

    c1 = reshape(b1, (/3,5/))

    print *, c1
    select type (b1)
        type is (Child)
            print *, b1
        class default
            error stop 1_4
    end select
end
