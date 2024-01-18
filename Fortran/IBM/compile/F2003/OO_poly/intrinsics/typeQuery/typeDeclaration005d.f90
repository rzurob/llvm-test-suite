!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!* TEST BUCKET                : OO_poly/intrinsics/typeQuery
!* PROGRAMMER                 : Yong Du
!* DATE                       : 10/25/2004
!* ORIGIN                     :
!* PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!* SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!* DRIVER STANZA              : xlf90
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
    type SequenceBase
        sequence
        integer i
        integer j
    end type
end module

program typeDeclaration005d
use m
    class(*), pointer :: arg1 => null()
    class(SequenceBase), pointer :: sb1 => null()

    if(extends_type_of(arg1, sb1)) error stop 1_4
    if(same_type_as(arg1, sb1)) error stop 2_4
end
