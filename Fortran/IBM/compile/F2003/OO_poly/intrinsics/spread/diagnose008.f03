! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/05/2004
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    The return value of spread is polymorphic and shall not be
!*  handled by regular IO.
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

program diagnose008
use m
    class(Base), pointer :: b1(:)
    allocate(b1(3), SOURCE=(/Base(1),Base(2),Base(3)/))
    print *, spread(b1, 1, 2)
end