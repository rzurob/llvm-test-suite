! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 291510)
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
    end type

    type, extends(base) :: child
        character(15) :: name
    end type

    type dataType
        class (base), allocatable :: data
    end type
end module

program fmisc010b1_0
use m
    type (dataType) :: d1 (10)

    d1 = (/(dataType(child(i, 'temp')), i = 1, 10)/)
end