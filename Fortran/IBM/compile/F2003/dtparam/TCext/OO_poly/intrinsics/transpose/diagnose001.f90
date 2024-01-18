! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/transpose/diagnose001.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/30/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    MATRIX is scalar. Non-poly.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type
end module

program diagnose001
use m
    type(Base(20,4)) :: b1
    b1 = Base(20,4)(1)
    print *, transpose(Base(20,4)(1))
    print *, transpose(b1)
end
