!***********************************************************************
!* =====================================================================
!* DATE                       : 10/26/2004
!* PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!* SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!* DESCRIPTION                :
!*   MOLD: non polymorphic
!*   A   : non polymorphic
!*   Either MOLD or A has declared type non extensible.
!*   Diagnostic test case.
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 05/19/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Removed TRUN header
!*                              2) Updated vf due to error msg change
!*                              3) Updated scenario file
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type SequenceBase
        sequence
        integer i
        integer j
    end type
end module

program typeDeclaration007d
use m
    type(SequenceBase) :: arg1
    type(SequenceBase) :: mold1
    if(extends_type_of(arg1, mold1)) error stop 1_4
    if(same_type_as(arg1, mold1)) error stop 2_4
end
