!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal505a.f
! %VERIFY: ffinal505a.out:ffinal505a.vf
! %STDIN:
! %STDOUT: ffinal505a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (allocated allocatables in the main
!*                               program not finalized)
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
        integer*4 :: id = -1

        contains

        final :: finalizeBase
        final :: finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type(base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

module m1
use m
    type dataType
        type (base), allocatable :: data1
        class (base), allocatable :: data2(:)
    end type
end module

program ffinal505a
use m1
    type (dataType) :: d1

    allocate (d1%data1, d1%data2(3))
end
