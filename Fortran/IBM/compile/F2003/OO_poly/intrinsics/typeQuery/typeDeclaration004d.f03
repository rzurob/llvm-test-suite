!***********************************************************************
!* =====================================================================
!* TEST BUCKET                : OO_poly/intrinsics/typeQuery
!* DATE                       : 10/26/2004
!* PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!* SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!* DESCRIPTION                :
!*   A   : unlimited polymorphic and is a disassociated pointer or an
!*         unallocated allocatable.
!*   MOLD: non polymorphic and declared type is not extensible.
!*   Diagnosic test case.
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

program typeDeclaration004d
use m
    class(*), pointer :: arg1 => null()
    type(SequenceBase) :: mold

    if(extends_type_of(arg1, mold)) error stop 1_4
    if(same_type_as(arg1, mold)) error stop 2_4
end
