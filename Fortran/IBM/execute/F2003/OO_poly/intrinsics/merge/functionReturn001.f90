! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE or FSOURCE is the return value of an internal function
!*  call.
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

program functionReturn001
use m
    logical :: m1(4,3)
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./), (/4,3/))

    select type(name1=>merge(func2(), func1(), m1))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    contains

    function func1()
        type(Child) :: func1(4,3)
        func1 = reshape((/(Child(i,i+1),i=1,23,2)/),(/4,3/))
    end function

    function func2()
        class(Base), pointer :: func2(:,:)
        allocate(func2(4,3), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
         (/4,3/)))
    end function
end
