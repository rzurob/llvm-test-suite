!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ffinal019a.f
! %VERIFY: ffinal019a.out:ffinal019a.vf  
! %STDIN:
! %STDOUT: ffinal019a.out 
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal019a.f
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
!*  DESCRIPTION                : testing final subroutines:defect 284803
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m   

   type :: base
      integer :: x
   end type

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
      print *, "finalizeChild"
   end subroutine

end module 

   use m1
 
   call example
 
 end
   
   subroutine example()
    
      use m1
     
      type(child) :: dt1

     allocate(dt0)
     deallocate(dt0)
  
   end subroutine

