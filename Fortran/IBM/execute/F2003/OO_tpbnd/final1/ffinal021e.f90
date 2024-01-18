!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS:  -qfree=f90
! %GROUP: ffinal021e.f
! %VERIFY: ffinal021e.out:ffinal021e.vf
! %STDIN:
! %STDOUT: ffinal021e.out
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal021e.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines:
!*                               derived type with volatile attribute.
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
    type(base), volatile :: dt1
    type(child), volatile  :: dt2

end subroutine
