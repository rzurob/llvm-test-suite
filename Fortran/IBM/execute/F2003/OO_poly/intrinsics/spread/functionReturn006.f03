! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is the return value of intrinsic function spread.
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
        integer :: i = -1
    end type

    type, extends(Base) :: Child
        integer :: j = -2
    end type
end module

program functionReturn006
use m
    class(*), allocatable :: b1(:,:,:)

    allocate(b1(2,2,2), SOURCE=reshape((/(Child(i,-i),i=1,8)/),(/2,2,2/)))

    select type(name1=>spread(spread(b1,4,3), 5, 2))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end