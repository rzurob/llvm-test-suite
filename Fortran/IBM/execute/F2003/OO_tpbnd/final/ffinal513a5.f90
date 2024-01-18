!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal513a5.f
! %VERIFY: ffinal513a5.out:ffinal513a5.vf
! %STDIN:
! %STDOUT: ffinal513a5.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization for structure component
!*                               in INTENT(OUT))
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

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        b%id = -1
    end subroutine
end module

module m1
use m, only : base
    type t
        type(base) :: b1 = base(0)
        type(base), allocatable :: b2
    end type

    contains

    subroutine abc (t1)
        type (t), intent(out) :: t1
    end subroutine
end module

program ffinal513a5
use m1
    type(t), save :: t1

    t1%b1%id = 100
    allocate (t1%b2)

    call abc (t1)

    if (t1%b1%id /= 0) error stop 1_4

    if (allocated (t1%b2)) error stop 2_4

    print *, '2nd test'

    call abc (t1)
end
