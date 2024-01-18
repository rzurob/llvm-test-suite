! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ASSOCIATE construct (array usage inside
!*                               ASSOCIATE construct; associate-name is array)
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
        integer*4 :: id
    end type
end module

program fpAssgn030a3_1
use m
    type (base) :: c(2:10)

    associate (x => c(::2))
        x%id = 10

        print *, x
    end associate
end
