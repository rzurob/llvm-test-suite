! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/30/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    MATRIX is array but not rank two. Non-poly.
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

program diagnose002
use m
    type(Base) :: b1(2,2,2)
    b1 = reshape((/(Base(i),i=1,8)/), (/2,2,2/))

    print *, transpose((/Base(1),Base(2)/))
    print *, transpose(b1)
end
