! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/spread/functionReturn002.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnostic test case. When return value of spread is poly,
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type
end module

program functionReturn002
use m
    class(Base(4)), allocatable :: b1(:,:)

    allocate(b1(3,4), SOURCE=reshape((/(Base(4)(i),i=1,12)/), (/3,4/)))

    print *, spread(b1, 3, 2)
end
