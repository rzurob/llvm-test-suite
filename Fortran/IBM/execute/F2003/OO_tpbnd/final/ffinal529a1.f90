!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal529a1.f
! %VERIFY: ffinal529a1.out:ffinal529a1.vf
! %STDIN:
! %STDOUT: ffinal529a1.out
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

        final :: finalizeBase
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        final :: finalizeChild
    end type

    type container
        class (base), allocatable :: data
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

    subroutine test1 (co)
        type (container), intent(OUT) :: co
    end subroutine

    subroutine test2 (co)
        class (container), intent(OUT) :: co
    end subroutine
end module

program ffinal529a1
use m
    class (container), pointer :: co1

    allocate (co1)

    allocate (co1%data)

    call test1 (co1)

    if (allocated (co1%data)) error stop 1_4

    print *, 'test2'

    allocate (child :: co1%data)

    call test2 (co1)

    if (allocated (co1%data)) error stop 2_4

    deallocate (co1)

    print *, 'end'
end
