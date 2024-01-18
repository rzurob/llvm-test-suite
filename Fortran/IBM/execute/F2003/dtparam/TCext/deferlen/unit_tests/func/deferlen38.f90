! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/deferlen/unit_tests/func/deferlen38.f
! opt variations: -qck -qnol

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Deferred Character Length
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the structure constructor related 
!*                               with characters with deferred length
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        character(:), allocatable ::id
        real(k1)      :: value(2)
    end type
end module

use m
    type (base(20,4)) :: b1
 
    character(:), allocatable :: char1
    allocate(character(4):: char1)
    char1 = '1234'
    b1 = base(20,4) (char1, (/1.0, 2.0/))
    if (b1%id /= '1234') error stop 1
    deallocate (char1)
end

