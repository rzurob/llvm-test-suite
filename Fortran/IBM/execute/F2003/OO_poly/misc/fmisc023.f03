! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 294591)
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

program fmisc023
    integer(8), target :: i1
    integer(8), pointer :: x

    x => i1

    associate (y => i1)
        if (.not. associated (x, y)) error stop 1_4
    end associate
end
