! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/08/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnostic test case: PAD shall not be scalar.
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

program diagnose005
use m
    class(Base), pointer :: b1(:)
    allocate(b1(15), SOURCE=(/(Base(i),i=1,15)/))

    print *, reshape(b1, (/3,6/), Base(1), (/1,2/))
end
