!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal020ak2.f
!*  TEST CASE NAME             : type-bound procedure ffinal020ak2
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal020a by Catherine Sun)
!*  DATE                       : 2007-11-26 (original: )
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines 
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : testing the dummy argument of
!*                               final subroutine:
!*                               with private, public attributes
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type ::  base
   contains
      final :: finalBase
   end type

contains
   subroutine finalBase(arg1)
      type(base),intent(inout) :: arg1
      public :: arg1
      print *, "finalizeBase"
   end subroutine

end module 

module m1
   use m
   
   type,extends(base) :: child (kchild) ! kchild=32
      integer, kind :: kchild
   contains
      final :: finalChild
   end type

   type(child(32)), allocatable :: dt0  ! tcx: (32)

contains
   subroutine finalChild(arg1)
      type(child(32)),intent(inout) :: arg1 ! tcx: (32)
      private :: arg1
      print *, "finalizeChild"
   end subroutine

end module 

   use m1
 
   call example
 
 end 
   
   subroutine example()
    
      use m1 
     
      type(base) :: dt1

      allocate(dt0)
      deallocate(dt0)
  
   end subroutine 



! Extensions to introduce derived type parameters:
! type: child - added parameters (kchild) to invoke with (32) / declare with (32) - 2 changes
