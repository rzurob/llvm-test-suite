
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS:  -qfree=f90
! %GROUP: ffinal021h.f
! %VERIFY: ffinal021h.out:ffinal021h.vf
! %STDIN:
! %STDOUT: ffinal021h.out
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal021h.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines:
!*                               derived type with allocatable attribute.
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
    type(base), allocatable :: dt1
    type(child),allocatable :: dt2

    print *, "example"

end subroutine
