! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous item (defect 294132)
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

program fmisc019
    type base
        integer(8), allocatable :: data(:)
    end type

    integer(8), allocatable :: i1(:)
    integer(8) k

    allocate (i1(0:7))

    i1 = (/(k,k=0_8,7_8)/)

    associate (x => base(i1(::2)), y => base(i1))
        print *, lbound(x%data), ubound(x%data)
        print *, lbound(y%data), ubound(y%data)
    end associate
end
