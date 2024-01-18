!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal012a.f
! %VERIFY: ffinal012a.out:ffinal012a.vf
! %STDIN:
! %STDOUT: ffinal012a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (no-autodeallocation of allocated
!*                               allocatables in the main program)
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
        integer :: x

        contains

        final :: finalizeBase
        final :: finalizeBaseArray
    end type

    type p
        type (base), allocatable :: b(:)
    end type

    contains
    subroutine finalizeBase (b1)
        type (base), intent(inout) :: b1
        print *, 'in finalizeBase'
    end subroutine

    subroutine finalizeBaseArray (b1)
        type (base), intent(in) :: b1(:)
        print *, 'in finalizeBaseArray'
    end subroutine

end module


program ffinal012a
use m

    type (p) :: p1

    allocate (p1%b(2))

end

