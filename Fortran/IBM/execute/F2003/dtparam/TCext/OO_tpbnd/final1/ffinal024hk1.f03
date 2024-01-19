!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-27 (original: )
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : testing final subroutines: import
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
    integer(kbase_1) :: int
    contains
       final :: finalizeBase
    end type
    contains
    subroutine finalizeBase (arg1)
       type (base(4)), intent (in) :: arg1  ! tcx: (4)
       print *, 'finalizeBase'
    end subroutine
end module

module m1
use m
    type, extends (base) :: child
    contains
       final :: finalizeChild
    end type

    class(base(4)), pointer  :: dt1   ! tcx: (4)

    interface interf
        subroutine fBase(arg1)
        import base
        class(base(4)), intent(inout) :: arg1 ! tcx: (4)
        end subroutine
    end interface

    contains

    subroutine finalizeChild (arg1)
        type (child(4)), intent (in) :: arg1  ! tcx: (4)
        print *, 'finalizeChild'
    end subroutine

end module

module m2
use m1
   type dt (kdt_1) ! kdt_1=4
      integer, kind :: kdt_1
      type(base(kdt_1)), allocatable ::  dt_b  ! tcx: (kdt_1)
      type(child(kdt_1)), pointer :: dt_c(:)  ! tcx: (kdt_1)
   end type
end module

use m2

    allocate (dt1)

    call interf(dt1)
    print *, "end of program"

end

subroutine fBase (arg1)
use m, only : base
use m2, only : dt
   type(base(4)), intent(inout) :: arg1 ! tcx: (4)
   class(dt(4)), allocatable  :: t1(:)   ! tcx: (4)
   allocate(t1(2))
   allocate(t1(1)%dt_b)
   allocate(t1(1)%dt_c(2))
   allocate(t1(2)%dt_b)
   allocate(t1(2)%dt_c(2))
   deallocate(t1(1)%dt_b, t1(1)%dt_c)
   deallocate(t1(2)%dt_b, t1(2)%dt_c)
end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
! type: child - added parameters () to invoke with (4) / declare with (4) - 2 changes
! type: dt - added parameters (kdt_1) to invoke with (4) / declare with (4) - 1 changes
