!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal023dkk.f
!*  TEST CASE NAME             : type-bound procedure ffinal023dkk
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal023d by Catherine Sun)
!*  DATE                       : 2007-11-26 (original: )
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
    type, extends (base) :: child (kchild) ! kchild=8
       integer, kind :: kchild
    contains
       final :: finalizeChild
    end type

    interface print
        subroutine fBase(arg1)
        import base
        integer, intent(in) :: arg1
        end subroutine
  
        subroutine fChild(arg1, arg2)
        import child
        integer, intent(in) :: arg1, arg2
        end subroutine
    end interface

    contains

    subroutine finalizeChild (arg1)
        type (child(4,8)), intent (in) :: arg1(:,:)  ! tcx: (4,8)
        print *, 'finalizeChild'
    end subroutine
 
end module

use m1

    call print(10)
    call print(10, 10)

end

subroutine fBase (arg1)
use m, only : base 
   integer, intent(in) :: arg1
   type(base(4)), allocatable  :: dt1   ! tcx: (4)
   allocate(dt1)
   deallocate(dt1)
end subroutine 

subroutine fChild (arg1, arg2)
use m1, only : child
    integer, intent(in) :: arg1, arg2
    type(child(4,8)), allocatable  :: dt1(:,:)  ! tcx: (4,8)
    allocate(dt1(2,2))
    deallocate(dt1)
end subroutine 



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (kchild) to invoke with (4,8) / declare with (4,8) - 2 changes
