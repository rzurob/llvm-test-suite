! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/OO_poly/intrinsics/typeQuery/structureConstructor002d.f
! opt variations: -ql -qreuse=none

!***********************************************************************
!* =====================================================================
!* TEST BUCKET                : OO_poly/intrinsics/typeQuery
!* DATE                       : 10/20/2004
!* PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!* SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!* DESCRIPTION                :
!*   A   : is unlimited polymorphic and is a disassociated pointer or
!*         an unallocated allocatable.
!*   MOLD: is specified using structure constructor with non extensible
!*         non abstract type.
!* =====================================================================
!* REVISION HISTORY
!*                   MM/DD/YY : 05/19/05
!*                       Init : yongdu@ca.ibm.com
!*                   Comments : 1) Removed TRUN header
!*                              2) Updated vf based on error msg change
!*                              3) Updated scenario file
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type SequenceBase(k1)    ! (4)
        integer, kind :: k1
        sequence
        integer(k1)      i
        integer(k1)      j
    end type
end module

program structureConstructor002d
use m
    class(*), pointer :: arg1 => null()
    if(extends_type_of(arg1, SequenceBase(4)(1, 2))) error stop 1_4
    if(same_type_as(arg1, SequenceBase(4)(1, 2))) error stop 2_4
end