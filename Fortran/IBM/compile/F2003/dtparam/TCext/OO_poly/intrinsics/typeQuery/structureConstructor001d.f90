! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_poly/intrinsics/typeQuery/structureConstructor001d.f
! opt variations: -qnol -qreuse=self

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!* TEST BUCKET                : OO_poly/intrinsics/typeQuery
!* PROGRAMMER                 : Yong Du
!* DATE                       : 10/20/2004
!* ORIGIN                     :
!* PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!* SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!* DRIVER STANZA              : xlf90
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
    type SequenceBase(n1,k1,k2)    ! (20,4,4)
        integer, kind :: k1,k2
        integer, len  :: n1
        sequence
        integer(k1)      i
        integer(k2)      j
    end type
end module

program structureConstructor001d
use m
    class(*), pointer :: mold1 => null()
    if(extends_type_of(SequenceBase(20,4,4)(1, 2), mold1)) error stop 1_4
    if(same_type_as(SequenceBase(20,4,4)(1, 2), mold1)) error stop 2_4
end
