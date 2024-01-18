!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal014a2.f
! %VERIFY: ffinal014a2.out:ffinal014a2.vf
! %STDIN:
! %STDOUT: ffinal014a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (poly-allocatable's finalization)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child) :: c(:)

        print *, 'finalizeChildRank1 and then', size (c), 'base parent comp.'
    end subroutine
end module

program ffinal014a2
use m
    type (child), allocatable :: b (:)
    class (base), allocatable :: a1, a2 (:)

    allocate (child :: b(2), a1, a2(3))

    print *, 'deallocating b'

    deallocate (b)

    print *, 'deallocating a1'

    deallocate (a1)

    print *, 'deallocating a2'

    deallocate (a2)

    print *, 'end'
end
