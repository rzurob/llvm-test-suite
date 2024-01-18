! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/02/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is the return value of
!*                               intrinsic function transpose().
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

program functionReturn006
use m
    type(Base) :: b1(10)
    b1 = (/ (Base(i), i=1,10) /)

    print *, transpose(reshape(b1,(/3,5/),(/Base(-1),Base(-2)/),(/2,1/)))
end
