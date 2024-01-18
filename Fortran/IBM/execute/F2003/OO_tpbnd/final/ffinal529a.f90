!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal529a.f
! %VERIFY: ffinal529a.out:ffinal529a.vf
! %STDIN:
! %STDOUT: ffinal529a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (allocated allocatable subobjects
!                               with INTENT(OUT) dummy-arg)
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

        final :: finalizeBase, finalizeBaseRank1
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    type container
        class (base), allocatable :: data (:)
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child), intent(in) :: c (:)

        print *, 'finalizeChildRank1'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine test1 (co)
        type (container), intent(OUT) :: co
    end subroutine

    subroutine test2 (co)
        class (container), intent(OUT) :: co
    end subroutine
end module

program ffinal529a
use m
    type (container) :: co1

    allocate (co1%data(3))

    call test1 (co1)

    if (allocated (co1%data)) error stop 1_4

    print *, 'test2'

    allocate (child :: co1%data(2))

    call test2 (co1)

    if (allocated (co1%data)) error stop 2_4

    print *, 'end'
end
