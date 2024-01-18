!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ffinal020a.f
! %VERIFY: ffinal020a.out:ffinal020a.vf  
! %STDIN:
! %STDOUT: ffinal020a.out 
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal020a.f
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : final subroutines 
!*
!*  SECONDARY FUNCTIONS TESTED : 
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
   
   type,extends(base) :: child 
   contains
      final :: finalChild
   end type

   type(child), allocatable :: dt0 

contains
   subroutine finalChild(arg1)
      type(child),intent(inout) :: arg1
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

