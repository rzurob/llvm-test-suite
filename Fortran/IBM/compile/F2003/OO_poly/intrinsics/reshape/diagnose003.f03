! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/08/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnostic test case: PAD shall be of the same type and type
!*  parameters as SOURCE. (Note: people are still debating this
!*  precondition. Maybe the correct precondition is: SOURCE shall
!*  be type compatible with PAD. Right now, only disgnose the case
!*  where SOURCE is not type compatible with PAD.)
!*    SOURCE is poly derived type and PAD is intrinsic type.
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

program diagnose003
use m
    class(Base), pointer :: b1(:)
    allocate(b1(15), SOURCE=(/(Base(i),i=1,15)/))

    print *, reshape(b1, (/3,6/), (/-1/), (/1,2/))
end
