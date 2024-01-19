! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnostic test case. When return value of merge is poly,
!*  it shall not be processed by regular IO.
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

program functionReturn002
use m
    class(Base), allocatable :: b1(:,:)

    allocate(b1(3,4), SOURCE=reshape((/(Base(i),i=1,12)/), (/3,4/)))

    print *, merge(b1, Base(1), .TRUE.)
end
