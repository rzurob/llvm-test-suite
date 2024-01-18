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
    type Base
        integer :: i = 9
    end type
end module

program spread001
use m
    type(Base) :: b1(2,3)
    b1 = reshape((/(Base(i),i=1,6)/), (/2,3/))

    print *, size(spread(b1, 1, 0))
    print *, shape(spread(b1, 1, 0))

    print *, size(spread(b1, 2, 0))
    print *, shape(spread(b1, 2, 0))

    print *, size(spread(b1, 3, 0))
    print *, shape(spread(b1, 3, 0))
end
