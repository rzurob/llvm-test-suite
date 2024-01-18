! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/spread/spread001.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/05/2004
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    The return is a zero-sized array if NCOPIES is 0.
!*    Non-poly.
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
        integer(k1)   :: i = 9
    end type
end module

program spread001
use m
    type(Base(20,4)) :: b1(2,3)
    b1 = reshape((/(Base(20,4)(i),i=1,6)/), (/2,3/))

    print *, size(spread(b1, 1, 0))
    print *, shape(spread(b1, 1, 0))

    print *, size(spread(b1, 2, 0))
    print *, shape(spread(b1, 2, 0))

    print *, size(spread(b1, 3, 0))
    print *, shape(spread(b1, 3, 0))
end
