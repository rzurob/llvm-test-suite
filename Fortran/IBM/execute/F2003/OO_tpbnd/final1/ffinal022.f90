!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS:  -qfree=f90
! %GROUP: ffinal022.f
! %VERIFY: ffinal022.out:ffinal022.vf
! %STDIN:
! %STDOUT: ffinal022.out
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal022.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines:
!*                               finalizations in  print statement
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

    interface interf
        function fBase()
        import base
           type(base) :: fBase
        end function

        function fChild(arg1)
        import child
           type(child) :: fChild
           integer, intent(in) :: arg1
        end function
    end interface

    contains

    subroutine finalizeChild (arg1)
        type (child), intent (in) :: arg1
        print *, 'finalizeChild'
    end subroutine

end module

use m1

    print *, interf()
    print *, interf(10)

end

function fBase ()
use m, only : base
   type(base) :: fBase
   fBase%int = 10
end function

function fChild (arg1)
use m1, only : child
    type (child)  :: fChild
    integer, intent(in) :: arg1
    fChild%int = arg1
end function

