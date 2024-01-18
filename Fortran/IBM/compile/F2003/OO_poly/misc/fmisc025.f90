! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 296612)
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

program fmisc025
    integer j(3), k(1)
    integer, parameter :: i1 = 10

    associate (x => 10)
        j = (/(x, x = 1, 3)/)   !<-- literal can not be ac-do-var
    end associate

    associate (x => i1)
        j = (/(x, x = 1, 3)/)   !<-- named const. can't be ac-do-var
    end associate

    associate (x => k)
        j = (/(x, x = 1, 3)/)   !<-- array can't be ac-do-var
    end associate

end
