! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnostic test case. When return value of transpose is poly,
!*  it shall not be processed by regular IO.
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

program functionReturn002
use m
    class(Base), allocatable :: b1(:,:)
    allocate(b1(2,4), SOURCE=reshape((/(Base(i),i=1,8)/), (/2,4/)))

    print *, transpose(b1)
end
