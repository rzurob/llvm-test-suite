! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/05/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is non-poly
!*    PAD and ORDER are not specified
!*    SOURCE is rank two.
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
end module

program reshape022
use m
    type(Base) :: b1(20)
    type(Base) :: b2(6,4)
    type(Base) :: b3(3,5)

    b1 = (/ (Base(i), i=1,20) /)

    b2 = reshape(b1, (/6,4/), (/Base(-1),Base(-2)/), (/2,1/))

    b3 = reshape(b2, (/3,5/), (/Base(-3),Base(-4)/), (/2,1/))

    print *, b1
    print *, b2
    print *, b3
end