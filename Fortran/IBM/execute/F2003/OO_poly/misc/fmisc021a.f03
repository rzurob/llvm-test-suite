! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellanous items (defect 294510)
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

program fmisc021a
    type A
        integer i, j
    end type

    type (a) a1(3)

    a1%i = (/1, 2, 3/)
    a1%j = (/10, 20, 30/)


    associate (x1 => cshift (a1, 1), x2 => eoshift (a1, 1, a(0,0)))
        print *, x1
        print *, x2
    end associate
    end