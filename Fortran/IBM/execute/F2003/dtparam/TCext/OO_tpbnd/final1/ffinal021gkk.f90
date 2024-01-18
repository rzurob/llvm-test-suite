!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal021gkk.f
!*  TEST CASE NAME             : type-bound procedure ffinal021gkk
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal021g by Catherine Sun)
!*  DATE                       : 2007-11-26 (original: )
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines 
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : testing final subroutines: 
!*                               derived type with pointer attribute.
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: x
    contains
        final :: finalizeBase
    end type
    
    type, extends(base) :: child (kchild) ! kchild=128
       integer, kind :: kchild
    contains
       final :: finalizeChild
    end type

    type(child(4,128)), allocatable :: t_c1 ! tcx: (4,128)

    contains
    subroutine finalizeBase (b1)
        type (base(4)), intent(in) :: b1 ! tcx: (4)
        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (b1)
        type (child(4,128)), intent(in) :: b1 ! tcx: (4,128)
        print *, 'finalizeChild'
    end subroutine

end module

    use m

    call example
end

subroutine example

use m
    type(base(4)), parameter :: dt1 = base(4)(2) ! tcx: (4) ! tcx: (4)
    type(child(4,128)),parameter :: dt2 = child(4,128)(3) ! tcx: (4,128) ! tcx: (4,128)
   
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (kchild) to invoke with (4,128) / declare with (4,128) - 4 changes
