! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/deferlen/unit_tests/func/deferlen39.f
! opt variations: -qck -ql

!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the structure constructor related
!*                               with characters with deferred length
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base(k1)    ! (4)
        integer, kind :: k1
        character(:), allocatable ::id
        real(k1)      :: value(2)
    end type
end module

use m
    type (base(4)) :: b1

    b1 = base(4) ('abcd', (/10.0, 3.0/))
    if (b1%id /= 'abcd') error stop 1
end

