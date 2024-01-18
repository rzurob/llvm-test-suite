! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/deferlen/unit_tests/func/deferlen31.f
! opt variations: -qck -qnok -qnol

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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
       character(:),allocatable :: firstName
    end type

    type, extends(base) :: child(k2,n2)    ! (4,20,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(:), allocatable :: lastName
    end type

end module

use m

    type (base(4,20)) :: b
    type (child(4,20,4,20)) :: c
    b = base(4,20)('Michael')
    c = child(4,20,4,20)(b%firstname, ' Harper')
    if (c%firstName /= 'Michael') error stop 1
    if (c%lastName /= ' Harper') error stop 2

end
