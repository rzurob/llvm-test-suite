! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (structure constructor
!*                               used as selector in ASSOCIATE construct)
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
        integer*4 :: id = 1
    end type
end module

program fconstr048
use m
    associate (x => base())
        if (x%id /= 1) error stop 1_4
    end associate

    associate (x => base (id = 9), x1 => base(10))
        if (x%id /= 9) error stop 2_4
        if (x1%id /= 10) error stop 3_4
    end associate
end