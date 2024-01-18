!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg005a8_1.f
! %VERIFY: fArg005a8_1.out:fArg005a8_1.vf
! %STDIN:
! %STDOUT: fArg005a8_1.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (unlimited
!                               poly-allocatable dummy-arg array to be
!                               associated only with unlimited poly-allocatable
!                               actual-arg array; use derived types to test the
!                               final binding calls)
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
    contains

    subroutine copyData (x, x1)
        class (*), allocatable, intent(out) :: x(:)
        class (*), intent(in) :: x1(:)

        allocate (x(size(x1)), source=x1)
    end subroutine
end module

module m1
    type base
        integer(4) id

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    type, extends (base) :: child
        character(20) name

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child), intent(in) :: c(:)

        print *, 'finalizeChildRank1'
    end subroutine
end module

program fArg005a8_1
use m
use m1
    type (base) :: b1 (3)
    class (*), allocatable :: x(:)

    allocate (child :: x(5))

    call copyData (x, b1)

    if (.not. allocated (x)) error stop 1_4

    if (size (x) /= 3) error stop 2_4

    print *, 'deallocating x'

    deallocate (x)
end
