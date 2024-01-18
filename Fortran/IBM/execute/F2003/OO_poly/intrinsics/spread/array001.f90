! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/06/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is array element or array section
!*    Poly and unlimited poly
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
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program array001
use m
    class(Base), allocatable :: b1(:,:)
    class(*), pointer :: u1(:,:)

    allocate(b1(2,3), SOURCE=reshape((/(Child(i,i+1),i=1,6)/), &
     (/2,3/)))
    allocate(u1(3,4), SOURCE=reshape((/(Child(i,i+1),i=1,23,2)/), &
     (/3,4/)))

    select type(name1=>spread(b1(2,3), 1, 3))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>spread(u1(:2,2:3), 3, 3))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
