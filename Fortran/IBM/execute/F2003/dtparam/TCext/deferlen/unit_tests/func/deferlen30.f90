! GB DTP extension using:
! ftcx_dtp -qck -ql /tstdev/F2003/deferlen/unit_tests/func/deferlen30.f
! opt variations: -qnock -qnol

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
       integer(k1)   :: id
    end type

    type, extends(base) :: child    ! (20,4)
        character(:), allocatable :: name
    end type

end module

use m

    type (child(20,4)) :: c1

    c1 = child(20,4)(1, 'Structure Constructor')
    if (c1%id /= 1) error stop 1
    if (c1%name /= 'Structure Constructor') error stop 2

    c1 = child(20,4)(1, 'Test Structure Constructor')
    if (c1%id /= 1) error stop 3
    if (c1%name /= 'Test Structure Constructor') error stop 4

end
