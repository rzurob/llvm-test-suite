!**********************************************************************
!*  ===================================================================
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

    class(base), pointer  :: dt1

    interface interf
        subroutine fBase(arg1)
        import base
        class(base), intent(inout) :: arg1
        end subroutine
    end interface

    contains

    subroutine finalizeChild (arg1)
        type (child), intent (in) :: arg1
        print *, 'finalizeChild'
    end subroutine

end module

module m2
use m1
   type dt
      type(base), allocatable ::  dt_b
      type(child), pointer :: dt_c
   end type
end module

use m2

    call interf(dt1)
    print *, "end of program"

end

subroutine fBase (arg1)
use m, only : base
use m2, only : dt
   class(base), intent(inout) :: arg1
   class(dt), allocatable  :: t1(:)
   allocate(t1(2))
   allocate(t1(1)%dt_b, t1(1)%dt_c)
   allocate(t1(2)%dt_b, t1(2)%dt_c)
   deallocate(t1(1)%dt_b, t1(1)%dt_c)
   deallocate(t1(2)%dt_b, t1(2)%dt_c)
end subroutine

