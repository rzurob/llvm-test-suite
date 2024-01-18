! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/06/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is function return of reshape.
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

program reshape003
use m
    class(AbstractParent), pointer :: c1(:,:,:)
    class(*), pointer :: b1(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(i,i-1),i=101,108)/), &
     (/2,2,2/)))

    select type(name1=>spread(reshape(c1, (/3,4/), &
     (/Child(-1,-2)/), (/2,1/)), 3, 2))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    allocate(b1(8), SOURCE=(/(Child(i,i-1),i=101,108)/))

    select type(name1=>spread(reshape(b1, (/2,2,2/)), 4, 3))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
