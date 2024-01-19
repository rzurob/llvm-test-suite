! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/21/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD is a structure component, which is non-poly
!*  array. The object containing the component is a scalar.
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
        type(Base) :: b1(20)
        type(Base) :: b2(5,5)
    end type

    type Base1
        integer i
        type(Base) :: j(2,2)
    end type
end module

program structureComponent001
use m
    type(Child) :: c1
    type(Base1) :: b1

    c1%b1 = (/ (Base(i), i=1,20) /)

    c1%b2 = reshape(c1%b1, (/5,5/), (/Base(-1),Base(-2)/), (/2,1/))

    associate(name1=>transfer(c1%b2, b1%j))
        print *, c1%b2
        print *, name1
        if(size(name1) .NE. 25) error stop 1_4
    end associate
end
