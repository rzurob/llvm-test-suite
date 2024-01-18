! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/04/2004
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    The rank of SOURCE shall be less than 20.
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
        integer :: i = 9
    end type
end module

program diagnose001
use m
    type(Base) :: b1(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
    print *, size(spread(b1, 1, 2))
end
