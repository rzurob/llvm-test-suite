! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/30/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    MATRIX is array but not rank two. Poly and unlimited poly.
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

program diagnose004
use m
    class(Base), pointer :: b1(:,:,:)
    class(*), allocatable :: b2(:,:)

    allocate(b1(2,2,2), SOURCE=reshape((/(Base(i),i=1,8)/), (/2,2,2/)))
    allocate(b2(2,4), SOURCE=reshape((/(Base(i),i=1,8)/), (/2,4/)))

    print *, transpose(b1)
    print *, transpose(reshape(b2,(/2,2,2/)))
end