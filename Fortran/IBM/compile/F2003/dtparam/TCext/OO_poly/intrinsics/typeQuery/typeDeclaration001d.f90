! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration001d.f
! opt variations: -qnol -qreuse=none

!***********************************************************************
!* =====================================================================
!* TEST BUCKET                : OO_poly/intrinsics/typeQuery
!* DATE                       : 10/25/2004
!* ORIGIN                     :
!* PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!* SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!* DESCRIPTION                :
!*   MOLD: unlimited polymorphic and is a disassociated pointer or an
!*         unallocated allocatable.
!*   A   : non polymorphic and declared type is not extensible.
!*         Diagnostic test case.
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
    type SequenceBase(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        sequence
        integer(k1)      i
        integer(k1)      j
    end type
end module

program typeDeclaration001d
use m
    class(*), pointer :: mold1 => null()
    type(SequenceBase(20,4)) :: sb1

    if(extends_type_of(sb1, mold1)) error stop 1_4
    if(same_type_as(sb1, mold1)) error stop 2_4
end
