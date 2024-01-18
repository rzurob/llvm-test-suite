!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal018clk.f
!*  TEST CASE NAME             : type-bound procedure ffinal018clk
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal018c by Catherine Sun)
!*  DATE                       : 2007-11-26 (original: )
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines 
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : testing final subroutines: derived
!*                               type without data components: this
!*                               testcase ICE with 20040414 driver 
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      type t1 (lt1) ! lt1=1
         integer, len :: lt1
      contains
         final :: finalize_t1
      end type

      type, extends(t1) :: t2 (kt2) ! kt2=1
         integer, kind :: kt2
      contains
         final :: finalize_t2 
      end type 

   contains
      subroutine finalize_t1(x)
         type(t1(*)), intent(inout) :: x ! tcx: (*)
         print *, "finalize_t1"
      end subroutine
 
      subroutine finalize_t2(x)
         type(t2(*,1)), intent(inout) :: x(:,:,:) ! tcx: (*,1)
         print *, "finalize_t2"
      end subroutine
   end module
   
   use mod
   call sub()

   end

   subroutine sub()
   use mod
   type(t2(:,1)), allocatable :: x(:,:,:) ! tcx: (:,1)
   allocate(t2(1,1)::x(1,2,2)) ! tcx: t2(1,1)
   deallocate(x)
   end subroutine 


! Extensions to introduce derived type parameters:
! type: t1 - added parameters (lt1) to invoke with (1) / declare with (*) - 1 changes
! type: t2 - added parameters (kt2) to invoke with (1,1) / declare with (*,1) - 2 changes
