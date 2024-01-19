! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/06/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Function return of spread is the SOURCE or MOLD of transfer.
!*    Poly and unlimited poly.
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

program transfer004
use m
    class(AbstractParent), pointer :: c1(:,:,:)
    class(*), pointer :: b1(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(i,i-1),i=101,108)/), &
     (/2,2,2/)))

    associate(name1=>transfer(spread(c1, 4, 2), (/Base(1)/)))
        if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    allocate(b1(8), SOURCE=(/(Base(i),i=101,108)/))

    associate(name1=>transfer(spread(b1, 1, 2), &
     spread((/Child(1,1),Child(2,2)/), 2, 3)))
        if(.NOT. same_type_as(name1, Child(1,2))) error stop 2_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
