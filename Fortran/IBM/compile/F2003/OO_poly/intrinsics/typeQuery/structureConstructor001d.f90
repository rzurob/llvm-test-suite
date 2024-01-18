!***********************************************************************
!* =====================================================================
!* TEST BUCKET                : OO_poly/intrinsics/typeQuery
!* DATE                       : 10/20/2004
!* PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!* SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!* DESCRIPTION                :
!*   MOLD: is unlimited polymorphic and is a disassociated pointer or
!*         an unallocated allocatable.
!*   A   : is specified using structure constructor with non extensible
!*         non abstract type.
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

program structureConstructor001d
use m
    class(*), pointer :: mold1 => null()
    if(extends_type_of(SequenceBase(1, 2), mold1)) error stop 1_4
    if(same_type_as(SequenceBase(1, 2), mold1)) error stop 2_4
end
