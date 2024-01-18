! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous item (defect 289666)
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

program fmisc012
    real(4) r1(10)

    associate (x => kind(r1), x1 => kind (1.00d0), x2 => kind((1.0d0, 2.0d0)), &
                x3 => kind ('abcd'))

        print *, x, x1, x2, x3
    end associate

    associate (x => kind (1.0q10))
        print *, x
    end associate
end
