! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/30/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : MATRIX is non-poly.
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

program transpose001
use m
    type(Base) :: b1(2,4)
    b1 = reshape((/(Base(i),i=1,8)/), (/2,4/))

    print *, transpose(reshape((/(Base(i),i=1,9)/), (/3,3/)))

    print *, transpose(b1)
end