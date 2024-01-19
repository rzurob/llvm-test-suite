! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/transpose/transpose001.f
! opt variations: -ql

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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type
end module

program transpose001
use m
    type(Base(4)) :: b1(2,4)
    b1 = reshape((/(Base(4)(i),i=1,8)/), (/2,4/))

    print *, transpose(reshape((/(Base(4)(i),i=1,9)/), (/3,3/)))

    print *, transpose(b1)
end
