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
    type base
       character(:),allocatable :: firstName
    end type

    type, extends(base) :: child
        character(:), allocatable :: lastName
    end type

end module

use m

    type (base) :: b
    type (child) :: c
    b = base('Michael')
    c = child(b%firstname, ' Harper')
    if (c%firstName /= 'Michael') error stop 1
    if (c%lastName /= ' Harper') error stop 2

end
