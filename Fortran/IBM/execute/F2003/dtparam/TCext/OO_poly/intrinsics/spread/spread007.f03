! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/spread/spread007.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/06/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Source is array and non-poly.
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

program spread007
use m
    type(Base(20,4)) :: b1(2,3)
    b1 = reshape((/(Base(20,4)(i),i=3,8)/), (/2,3/))

    print *, spread((/Base(20,4)(1),Base(20,4)(2)/), 1, 3)
    print *, size(spread((/Base(20,4)(1),Base(20,4)(2)/), 1, 3))
    print *, shape(spread((/Base(20,4)(1),Base(20,4)(2)/), 1, 3))

    print *, spread(b1, 1, 4)
    print *, size(spread(b1, 1, 4))
    print *, shape(spread(b1, 1, 4))
end