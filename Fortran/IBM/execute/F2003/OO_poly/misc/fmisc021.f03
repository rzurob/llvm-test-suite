! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 294510)
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

program fmisc021
    type A
        integer i, j
    end type

    type (a) a1(3)

    a1%i = (/1, 2, 3/)
    a1%j = (/10, 20, 30/)

    print *, cshift (a1, 1)
    print *, eoshift (a1, 1, a(0,0))

end
