! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (array component with
!                               default initialization)
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

program fconstr503
    type base
        integer(4) :: i1(2) = (/1,2/)
    end type

    print *, base()
    associate (x => base())
        print *, x
        if (any(x%i1 /= (/1,2/))) error stop 1_4
    end associate
end
