! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (non-pure procedure cannot
!                               be used in FORALL construct)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4) :: id

        contains

        procedure :: getID => getBaseID
    end type

    contains

    integer(4) function getBaseID (b)
        class (base), intent(in) :: b

        getBaseID = b%id
    end function
end module

program ftpbnd521d1
use m
    type (base) :: b1(10)

    b1%id = 1

    FORALL (i=1:10, b1(i)%getID() < 5)
        b1(i) = base(b1(i)%getID() * 2)
    end forall
end
