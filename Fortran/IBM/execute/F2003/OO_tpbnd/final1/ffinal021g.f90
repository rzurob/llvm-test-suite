!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS:  -qfree=f90
! %GROUP: ffinal021g.f
! %VERIFY: ffinal021g.out:ffinal021g.vf
! %STDIN:
! %STDOUT: ffinal021g.out
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal021g.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines:
!*                               derived type with pointer attribute.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: x
    contains
        final :: finalizeBase
    end type

    type, extends(base) :: child
    contains
       final :: finalizeChild
    end type

    type(child), allocatable :: t_c1

    contains
    subroutine finalizeBase (b1)
        type (base), intent(in) :: b1
        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (b1)
        type (child), intent(in) :: b1
        print *, 'finalizeChild'
    end subroutine

end module

    use m

    call example
end

subroutine example

use m
    type(base), parameter :: dt1 = base(2)
    type(child),parameter :: dt2 = child(3)

end subroutine
