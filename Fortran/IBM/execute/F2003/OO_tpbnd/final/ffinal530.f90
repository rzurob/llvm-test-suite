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
! %GROUP: ffinal530.f
! %VERIFY: ffinal530.out:ffinal530.vf
! %STDIN:
! %STDOUT: ffinal530.out
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
!*  DATE                       : 06/21/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : final sub (allocated allocatable subojects in
!                               intrinsic assignment: NOTE it is possible to
!                               observe the order change of the finalization of
!                               two allocatable components, data1 and data2.  If
!                               that happens; update the expected results.)
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
        integer(4) :: id = -1

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    type, extends(base) :: child
        character(20) :: name = 'default'

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    type container1
        type (base), allocatable :: data1
        type (base), allocatable :: data2(:)
    end type

    type container2
        type (child), allocatable :: data1
        type (child), allocatable :: data2(:)
    end type

    contains

    subroutine finalizeChild (c)
        type (child) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child) :: c(:)

        print *, 'finalizeChildRank1'
    end subroutine

    subroutine finalizeBase(b)
        type (base), intent (in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent (in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program ffinal530
use m
    type (container1) :: co1a, co1b
    type (container2) :: co2a, co2b

    allocate (co1a%data1, co1a%data2(2:3), co1b%data1)

    co1b = co1a

    if ((lbound(co1b%data2,1) /= 2) .or. (ubound(co1b%data2,1) /= 3)) error stop 1_4

    print *, 'assignment the second time'

    co1b = co1a

    allocate (co2a%data1, co2a%data2 (0:2), co2b%data2(2))

    print *, 'the third assignment'

    co2b = co2a

    if ((lbound(co2b%data2,1) /= 0) .or. (ubound(co2b%data2,1) /= 2)) error stop 2_4

    print *, 'the last assignment'

    co2b = co2a

    print *, 'end'
end
