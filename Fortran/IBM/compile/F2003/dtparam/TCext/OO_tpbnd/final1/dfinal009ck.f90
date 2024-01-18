!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal009ck.f
!*  TEST CASE NAME             : type-bound procedure dfinal009ck
!*
!*  PROGRAMMER                 : David Forster (derived from dfinal009c by Catherine Sun)
!*  DATE                       : 2007-11-13 (original: )
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines 
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : testing the dummy argument of final 
!*                               subroutines: 
!*                               with value attribute
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
 
module m

   type :: base
   end type

end module 

module m1
   use m
   
   type,extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
   integer(kchild_1) :: x
   contains
      final :: finalChild 
   end type

   type(child(4)), allocatable :: dt0  ! tcx: (4)

contains
   subroutine finalChild(arg1)
      type(child(4))  :: arg1 ! tcx: (4)
      value :: arg1
      print *, "finalizeChild"
   end subroutine

end module

   use m1
 
   call example
 
 end 
   
   subroutine example()
    
      use m1
     
      type(child(4)) :: dt1 ! tcx: (4)

      allocate(dt0)
      deallocate(dt0)

   end subroutine 



! Extensions to introduce derived type parameters:
! type: child - added parameters (kchild_1) to invoke with (4) / declare with (4) - 3 changes
