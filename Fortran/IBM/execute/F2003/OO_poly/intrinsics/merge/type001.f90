! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Declared types of TSOURCE and FSOURCE are different, but their
!*  dynamic types are the same.
!*    TSOURCE is scalar or array
!*    FSOURCE is scalar or array
!*    MASK is scalar or array
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
        integer :: i = 8
    end type

    type, extends(Base) :: Child
        integer :: j = 9
    end type
end module

program type001
use m
    class(Base), pointer :: b1
    type(Child) :: b2

    allocate(b1, SOURCE=Child(3,4))
    b2%i = 5
    b2%j = 6

    select type(name1=>merge(b1, b2, .TRUE.))
        type is (Child)
            print *, name1
        class default
            error stop 1_4
    end select

    associate(name1=>merge(b2, b1, &
     reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE./),(/2,2/))))
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
