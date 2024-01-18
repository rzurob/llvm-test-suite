!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal018ak2.f
!*  TEST CASE NAME             : type-bound procedure ffinal018ak2
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal018a by Catherine Sun)
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

      type, extends(t1) :: t2 (kt2) ! kt2=4
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
         type(t2(4)), intent(inout) :: x(1:3) ! tcx: (4)
         print *, "finalize_t2"
      end subroutine
   end module
   
   use mod
   call sub()

   end
   
   subroutine sub()
   use mod
   type(t2(4)), allocatable :: x(:) ! tcx: (4)
   allocate(x(1:3))
   deallocate(x)
   end subroutine


! Extensions to introduce derived type parameters:
! type: t2 - added parameters (kt2) to invoke with (4) / declare with (4) - 2 changes
