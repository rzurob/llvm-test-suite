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
       integer*4 :: id
    end type

    type, extends(base) :: child
        character(:), allocatable :: name
    end type

end module

use m

    type (child) :: c1

    c1 = child(1, 'Structure Constructor')
    if (c1%id /= 1) error stop 1
    if (c1%name /= 'Structure Constructor') error stop 2

    c1 = child(1, 'Test Structure Constructor')
    if (c1%id /= 1) error stop 3
    if (c1%name /= 'Test Structure Constructor') error stop 4

end
