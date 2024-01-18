!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f 
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: dcomp dfinal009c.f
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal009c.f
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
   
   type,extends(base) :: child 
   integer :: x
   contains
      final :: finalChild 
   end type

   type(child), allocatable :: dt0 

contains
   subroutine finalChild(arg1)
      type(child)  :: arg1
      value :: arg1
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

