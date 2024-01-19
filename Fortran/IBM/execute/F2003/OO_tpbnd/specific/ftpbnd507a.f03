! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (nopass binding with
!*                               dummy-args; called by an array)
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
        contains

        procedure, nopass :: typeID => baseID
        procedure, nopass, non_overridable :: typeTell
    end type

    private printBase, baseID, typeTell

    contains

    integer*4 function baseID ()
        baseID = 1
    end function

    integer*4 function typeTell (b)
        class (base), intent(in) :: b

        typeTell = b%typeID()
    end function
end module

program ftpbnd507a
use m
    type (base) :: b1, b2(10)

    print *, b1%typeTell (b2(3))
    print *, b2%typeTell (b1)
end
