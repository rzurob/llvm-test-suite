! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/02/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is the return value of
!*                               intrinsic function transfer().
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
        integer :: i = 1
    end type

    type, extends(Base) :: Child
        integer :: j = 2
        integer :: k = 3
        integer :: l = 4
        integer :: m = 5
    end type
end module

program functionReturn005
use m
    type(Child) :: b1
    class(*), pointer :: arg1(:,:) => null()
    b1%i = 8
    b1%j = 9

    allocate(arg1(2,2), SOURCE=reshape(transfer(b1,Base(1),5),(/2,2/)))

    select type (arg1)
        type is (Base)
            print *, arg1
        class default
            error stop 1_4
    end select
end
