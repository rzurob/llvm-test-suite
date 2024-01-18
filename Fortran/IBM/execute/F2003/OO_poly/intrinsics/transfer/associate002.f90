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

program associate002
use m
    class(AbstractParent), pointer :: ap1(:,:) => null()
    type(Child) :: c1(5)

    allocate(ap1(4,5), SOURCE=reshape((/(Base(i),i=1,20)/), (/4,5/)))

    associate(name1=>ap1(:,3))
        associate(name2=>transfer(name1, c1))
            if(.NOT. same_type_as(name2, c1)) error stop 1_4
            print *, size(name2)
            print *, name2
        end associate
    end associate

    associate(name1=>ap1(2:4,2:3))
        associate(name2=>transfer(name1, c1, 2))
            if(.NOT. same_type_as(name2, c1)) error stop 2_4
            print *, size(name2)
            print *, name2
        end associate
    end associate
end
