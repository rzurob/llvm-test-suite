! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is poly
!*    Assigned data entity is non-poly
!*    PAD and ORDER are specified. PAD has same declared type as SOURCE
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
        integer :: i = 88
    end type

    type, extends(Base) :: Child
        integer :: j = 99
    end type
end module

program reshape011
use m
    class(Base), pointer :: b1(:) => null()
    type(Base) :: c1(3,5)
    allocate(b1(10), SOURCE = (/ (Child(i,i+100), i=1,10) /))

    c1 = reshape(b1, (/3,5/), (/Child(-1,1),Child(-2,2)/), (/2,1/))

    print *, c1
    select type (b1)
        type is (Child)
            print *, b1
        class default
            error stop 1_4
    end select
end