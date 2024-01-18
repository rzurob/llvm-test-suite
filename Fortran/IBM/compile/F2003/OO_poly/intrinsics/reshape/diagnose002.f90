! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnostic test case: SHAPE shall be of type integer, rank one,
!*  and constant size.
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
    class(Base), pointer :: b1(:)
    allocate(b1(15), SOURCE=(/(Base(i),i=1,15)/))

    print *, reshape(b1, (/3.0,5.0/))
end
