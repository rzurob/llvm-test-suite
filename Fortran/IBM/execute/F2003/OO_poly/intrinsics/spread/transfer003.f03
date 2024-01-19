! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/06/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is function return of transfer.
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

program transfer003
use m
    class(AbstractParent), pointer :: c1(:,:,:)
    class(*), pointer :: b1(:)
    class(*), pointer :: b2
    class(*), pointer :: c2(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(i,i-1),i=101,108)/), &
     (/2,2,2/)))
    allocate(Base::b2)
    allocate(Child::c2(2))

    select type(name1=>spread(transfer(c1, b2, 5), 1, 2))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    allocate(b1(8), SOURCE=(/(Base(i),i=101,108)/))

    select type(name1=>spread(transfer(b1, c2), 1, 2))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
