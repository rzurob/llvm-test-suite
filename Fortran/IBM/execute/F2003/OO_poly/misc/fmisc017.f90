! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 286299)
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

program fmisc017
use m
    type (base) :: c(2:10)

    associate (x => c)
        x%id = 10

        print *, x
    end associate
end

