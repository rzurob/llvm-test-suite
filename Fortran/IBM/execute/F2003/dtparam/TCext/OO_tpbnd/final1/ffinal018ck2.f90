!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal018ck2.f
!*  TEST CASE NAME             : type-bound procedure ffinal018ck2
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
      type t1
      contains
         final :: finalize_t1
      end type

      type, extends(t1) :: t2 (kt2) ! kt2=8
         integer, kind :: kt2
      contains
         final :: finalize_t2 
      end type 

   contains
      subroutine finalize_t1(x)
         type(t1), intent(inout) :: x
         print *, "finalize_t1"
      end subroutine
 
      subroutine finalize_t2(x)
         type(t2(8)), intent(inout) :: x(:,:,:) ! tcx: (8)
         print *, "finalize_t2"
      end subroutine
   end module
   
   use mod
   call sub()

   end

   subroutine sub()
   use mod
   type(t2(8)), allocatable :: x(:,:,:) ! tcx: (8)
   allocate(x(1,2,2))
   deallocate(x)
   end subroutine 


! Extensions to introduce derived type parameters:
! type: t2 - added parameters (kt2) to invoke with (8) / declare with (8) - 2 changes
