!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal024ekk3.f
!*  TEST CASE NAME             : type-bound procedure ffinal024ekk3
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal024e by Catherine Sun)
!*  DATE                       : 2007-11-27 (original: )
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines 
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
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
    type, extends (base) :: child (kchild) ! kchild=4
       integer, kind :: kchild
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
        type (child(4,4)), intent (in) :: arg1  ! tcx: (4,4)
        print *, 'finalizeChild'
    end subroutine
 
end module

module m2
use m1
   type dt (kdt_1,kdt_2,kdt_3) ! kdt_1,kdt_2,kdt_3=4,4,4
      integer, kind :: kdt_1,kdt_2,kdt_3
      type(base(kdt_1)), allocatable ::  dt_b  ! tcx: (kdt_1)
      type(child(kdt_2,kdt_3)), pointer :: dt_c  ! tcx: (kdt_2,kdt_3)
   end type
end module

use m2

    call interf(dt1)
    print *, "end of program"

end

subroutine fBase (arg1)
use m, only : base
use m2, only : dt
   class(base(4)), intent(inout) :: arg1 ! tcx: (4)
   type(dt(4,4,4))  :: t1   ! tcx: (4,4,4)
   allocate(t1%dt_b, t1%dt_c)
   deallocate(t1%dt_b, t1%dt_c)
end subroutine 



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
! type: child - added parameters (kchild) to invoke with (4,4) / declare with (4,4) - 2 changes
! type: dt - added parameters (kdt_1,kdt_2,kdt_3) to invoke with (4,4,4) / declare with (4,4,4) - 1 changes
