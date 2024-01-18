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
    type base
        character(:), allocatable ::id
        real*4 :: value(2)
    end type
end module

use m
    type (base) :: b1

    b1 = base ('abcd', (/10.0, 3.0/))
    if (b1%id /= 'abcd') error stop 1
end

