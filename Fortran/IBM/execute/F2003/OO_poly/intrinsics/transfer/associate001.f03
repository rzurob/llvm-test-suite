! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/21/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is an associate name.
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
    end type
end module

program associate001
use m
    class(AbstractParent), pointer :: ap1(:) => null()

    allocate(ap1(20), SOURCE=(/ (Base(i),i=1,20) /))

    associate(name1=>ap1)
        associate(name2=>transfer(name1, Child(8,9)))
            if(.NOT. same_type_as(name2, Child(8,9))) error stop 1_4
            print *, name2
        end associate
    end associate

    associate(name1=>ap1(15:19))
        associate(name2=>transfer(name1, Child(8,9)))
            if(.NOT.  same_type_as(name2, Child(8,9))) error stop 2_4
            print *, name2
        end associate
    end associate
end
