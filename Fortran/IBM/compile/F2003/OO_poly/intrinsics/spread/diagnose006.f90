! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/04/2004
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    NCOPIES shall be scalar and of type integer.
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
        integer :: i = -1
    end type
end module

program diagnose006
use m
    type(Base) :: b1(2,2)
    print *, spread(b1, 1, 2.0)
end
