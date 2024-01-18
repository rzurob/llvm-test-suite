!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal009dll.f
!*  TEST CASE NAME             : type-bound procedure dfinal009dll
!*
!*  PROGRAMMER                 : David Forster (derived from dfinal009d by Catherine Sun)
!*  DATE                       : 2007-11-13 (original: )
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines 
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : testing the dummy argument of final 
!*                               subroutines must not be polymorphic.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
 
module m

   type :: base (lb) ! lb=0
      integer, len :: lb
   end type

end module 

module m1
   use m
   
   type,extends(base) :: child (lc) ! lc=0
      integer, len :: lc
   contains
!* expect error message 1514-597 here
      final :: finalChild 
   end type

   type(child(:,:)), allocatable :: dt0  ! tcx: (:,:)

contains
   subroutine finalChild(arg1)
      class(child(*,*)),intent(inout)  :: arg1 ! tcx: (*,*)
      private :: arg1
      print *, "finalizeChild"
   end subroutine

end module

   use m1
 
   call example
 
 end 
   
   subroutine example()
    
      use m1
     
      type(child(0,0)) :: dt1 ! tcx: (0,0)

      allocate(child(0,0)::dt0) ! tcx: child(0,0)
      deallocate(dt0)

   end subroutine 



! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (0) / declare with (*) - 0 changes
! type: child - added parameters (lc) to invoke with (0,0) / declare with (*,*) - 3 changes
