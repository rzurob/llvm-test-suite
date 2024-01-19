! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/10/2005
!*
!*  DESCRIPTION                : final sub (finalization of LHS during intrinsic
!                               assignment)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 id

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        b%id = 0
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        print *,'finalizeChild'

        c%name = ''
    end subroutine
end module

program ffinal501a
use m
    type (child) :: c1
    type (base) :: b1

    b1%id = 1

    c1%id = 10
    c1%name = 'c1_test'

    c1%base = b1   !<-- this should print out finalizeBase

    if (c1%id /= 1) error stop 1_4

    print *, 'end'
end
