! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (C633, kind type paramter for
!                               source-expr shall be the same as that of the
!                               allocate object)
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

program falloc003d3
    integer(4), pointer :: i1
    allocate (i1, source=9876543210_8)      !<-- illegal
    print *, i1
end
