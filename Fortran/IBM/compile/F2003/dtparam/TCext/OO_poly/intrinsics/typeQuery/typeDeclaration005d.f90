! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration005d.f
! opt variations: -ql -qreuse=self

!***********************************************************************
!* =====================================================================
!* TEST BUCKET                : OO_poly/intrinsics/typeQuery
!* DATE                       : 10/25/2004
!* ORIGIN                     :
!* PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!* SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!* DESCRIPTION                :
!*   A   : unlimited polymorphic and is a disassociated pointer or an
!*         unallocated allocatable.
!*   MOLD: polymorphic but not unlimited polymorphic. Declared type is
!*         non extensible. Diagnostic test case.
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
    type SequenceBase(k1,k2)    ! (4,4)
        integer, kind :: k1,k2
        sequence
        integer(k1)      i
        integer(k2)      j
    end type
end module

program typeDeclaration005d
use m
    class(*), pointer :: arg1 => null()
    class(SequenceBase(4,4)), pointer :: sb1 => null()

    if(extends_type_of(arg1, sb1)) error stop 1_4
    if(same_type_as(arg1, sb1)) error stop 2_4
end
