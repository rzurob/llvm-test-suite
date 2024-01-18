!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal009k.f
!*  TEST CASE NAME             : type-bound procedure dfinal009k
!*
!*  PROGRAMMER                 : David Forster (derived from dfinal009 by Catherine Sun)
!*  DATE                       : 2007-11-12 (original: )
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines 
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : testing final subroutines: 
!*                               The dummy argument of the final
!*                               subroutine finalchild must not have
!*                               the OPTIONAL attribute 
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
 
module m

   type :: base (kb) ! kb=4
      integer, kind :: kb
   end type

end module 

module m1
   use m
   
   type,extends(base) :: child 
   contains
      final :: finalChild 
   end type

   type(child(4)), allocatable :: dt0  ! tcx: (4)

contains
   subroutine finalChild(arg1)
      type(child(4)),intent(inout), optional :: arg1 ! tcx: (4)
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
