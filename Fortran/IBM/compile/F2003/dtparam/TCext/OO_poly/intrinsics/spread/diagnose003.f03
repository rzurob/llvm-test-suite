! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/spread/diagnose003.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/04/2004
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    DIM shall be scalar and of type integer with value in the range
!*  of 1<=DIM<=n+1, where n is th rank of SOURCE.
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
        integer(k1)   :: i = -1
    end type
end module

program diagnose003
use m
    type(Base(20,4)) :: b1(2,2)
    print *, spread(b1, 0, 2)
end