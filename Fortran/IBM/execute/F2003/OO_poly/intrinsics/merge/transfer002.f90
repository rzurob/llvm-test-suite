! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Function return of merge is the SOURCE or MOLD of transfer.
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

program transfer002
use m
    class(AbstractParent), pointer :: c1(:,:)
    class(*), allocatable :: b1(:,:)
    logical :: m1(2,2)

    allocate(c1(2,2), SOURCE=reshape((/(Child(i,i-1),i=101,104)/), &
     (/2,2/)))
    allocate(b1(2,2), SOURCE=reshape((/(Child(i,i+1),i=1,7,2)/), &
     (/2,2/)))
    m1 = reshape((/.FALSE.,.TRUE.,.FALSE.,.TRUE./), (/2,2/))

    associate(name1=>transfer(merge(c1, b1, m1), (/Base(1)/)))
        if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    select type(name1=>transfer((/(Base(i),i=1,20)/), &
     merge(b1, c1, m1)))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
