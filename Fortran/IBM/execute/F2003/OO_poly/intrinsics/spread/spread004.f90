! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/06/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Source is scalar and non-poly.
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

program spread004
use m
    type(Base) :: b1
    b1%i = 8

    print *, spread(Base(1), 1, 3)
    print *, size(spread(Base(1), 1, 3))
    print *, shape(spread(Base(1), 1, 3))

    print *, spread(b1, 1, 3)
    print *, size(spread(b1, 1, 3))
    print *, shape(spread(b1, 1, 3))
end
