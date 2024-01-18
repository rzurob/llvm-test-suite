!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal024k2.f
!*  TEST CASE NAME             : type-bound procedure ffinal024k2
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

    interface interf
        subroutine fBase(arg1)
        import base
        type(base(4)), intent(inout) :: arg1 ! tcx: (4)
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
   type dt (kdt_1,kdt_2) ! kdt_1,kdt_2=4,4
      integer, kind :: kdt_1,kdt_2
      type(base(kdt_1)) ::  dt_b  ! tcx: (kdt_1)
      type(child(kdt_2)) :: dt_c  ! tcx: (kdt_2)
   end type
end module

use m2

    call interf(dt1)

end

subroutine fBase (arg1)
use m, only : base
use m2, only : dt
   type(base(4)), intent(inout) :: arg1 ! tcx: (4)
   type(dt(4,4))  :: t1   ! tcx: (4,4)
end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
! type: child - added parameters () to invoke with (4) / declare with (4) - 2 changes
! type: dt - added parameters (kdt_1,kdt_2) to invoke with (4,4) / declare with (4,4) - 1 changes
