! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/OO_poly/intrinsics/typeQuery/typeDeclaration007d.f
! opt variations: -qnol -qreuse=none

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!* TEST CASE TITLE            :
!* PROGRAMMER                 : Yong Du
!* DATE                       : 10/26/2004
!* ORIGIN                     :
!* PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!* SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!* DRIVER STANZA              : xlf90
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
    type SequenceBase(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        sequence
        integer(k1)      i
        integer(k1)      j
    end type
end module

program typeDeclaration007d
use m
    type(SequenceBase(20,4)) :: arg1
    type(SequenceBase(20,4)) :: mold1
    if(extends_type_of(arg1, mold1)) error stop 1_4
    if(same_type_as(arg1, mold1)) error stop 2_4
end
