! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/25/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is an associate name. Selector
!*    is array section and is a structure component.
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
        integer j
        type(Base) :: b(5,6)
    end type
end module

program associate004
use m
    class(AbstractParent), pointer :: ap1(:,:,:) => null()
    type(Child) :: c1
    c1%i = 8
    c1%j = 9
    c1%b = reshape((/ (Base(i), i=1,30) /), (/5,6/))

    associate(name1=>c1%b(1:5:2,2:6:2))
        allocate(ap1(2,3,2), SOURCE= reshape(name1, &
         (/2,3,2/), (/Base(-1),Base(-2)/), (/1,2,3/)))
    end associate

    select type (ap1)
        type is (Base)
            print *, ap1
        class default
            error stop 1_4
    end select
end
