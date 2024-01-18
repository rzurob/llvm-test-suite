!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal023k.f
!*  TEST CASE NAME             : type-bound procedure ffinal023k
!*
!*  DATE                       : 2007-11-26 (original: )
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

    type(base(4))  :: dt1   ! tcx: (4)
    type(child(4))  :: dt2   ! tcx: (4)

    interface interf
        subroutine fBase(arg1)
        import base
        type(base(4)), intent(inout) :: arg1 ! tcx: (4)
        end subroutine

        subroutine fChild(arg1)
        import child
        type(child(4)), intent(inout) :: arg1 ! tcx: (4)
        end subroutine
    end interface

    contains

    subroutine finalizeChild (arg1)
        type (child(4)), intent (in) :: arg1  ! tcx: (4)
        print *, 'finalizeChild'
    end subroutine

end module

use m1

    call interf(dt1)
    call interf(dt2)

end

subroutine fBase (arg1)
use m, only : base
   type(base(4)), intent(inout) :: arg1 ! tcx: (4)
   type(base(4))  :: t1   ! tcx: (4)
end subroutine

subroutine fChild (arg1)
use m1, only : child
    type(child(4)), intent(inout) :: arg1 ! tcx: (4)
    type(child(4))  :: t2  ! tcx: (4)
end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
! type: child - added parameters () to invoke with (4) / declare with (4) - 5 changes
