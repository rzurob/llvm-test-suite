! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/02/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is the return value of an
!*                               external function call. Use select
!*                               type to check the return value.
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
        integer j
    end type

    contains

    function func1()
        class(Base), pointer :: func1(:)
        allocate(func1(10), SOURCE=(/ (Child(i+1,i-1), i=1,10) /))
    end function
end module

program functionReturn003
use m
    class(Base), allocatable :: b1(:)
    allocate(b1(2), SOURCE=(/Child(-1,1),Child(-2,2)/))
    select type (c1 => reshape(func1(), (/3,5/), b1, (/2,1/)))
        type is (Child)
            print *, c1
        class default
            error stop 1_4
    end select
end
