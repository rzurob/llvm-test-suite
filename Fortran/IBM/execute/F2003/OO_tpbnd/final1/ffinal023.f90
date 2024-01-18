!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS:  -qfree=f90
! %GROUP: ffinal023.f
! %VERIFY: ffinal023.out:ffinal023.vf
! %STDIN:
! %STDOUT: ffinal023.out
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal023.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines: import
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
    integer :: int
    contains
       final :: finalizeBase
    end type
    contains
    subroutine finalizeBase (arg1)
       type (base), intent (in) :: arg1
       print *, 'finalizeBase'
    end subroutine
end module

module m1
use m
    type, extends (base) :: child
    contains
       final :: finalizeChild
    end type

    type(base)  :: dt1
    type(child)  :: dt2

    interface interf
        subroutine fBase(arg1)
        import base
        type(base), intent(inout) :: arg1
        end subroutine

        subroutine fChild(arg1)
        import child
        type(child), intent(inout) :: arg1
        end subroutine
    end interface

    contains

    subroutine finalizeChild (arg1)
        type (child), intent (in) :: arg1
        print *, 'finalizeChild'
    end subroutine

end module

use m1

    call interf(dt1)
    call interf(dt2)

end

subroutine fBase (arg1)
use m, only : base
   type(base), intent(inout) :: arg1
   type(base)  :: t1
end subroutine

subroutine fChild (arg1)
use m1, only : child
    type(child), intent(inout) :: arg1
    type(child)  :: t2
end subroutine

