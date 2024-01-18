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
! %POSTCMD: dcomp ftybn093b.f ftybn093b.vf 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn093b.f 
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute 
!*
!*  SECONDARY FUNCTIONS TESTED : overriding 
!*
!*  DESCRIPTION                : The overriding binding and the overriden 
!*                               binding shall satisfy the following
!*                               condition: both shall be functions. 
!*                             
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod	      
      type base 
         integer :: x
	 contains
      	 procedure, nopass :: bind_b => proc1
      end type

      type, extends(base) :: parent1
      contains
         procedure, nopass :: bind_b => proc2
      end type  

      type, extends(base) :: parent2
      contains
!* expecting the error message 1514-631 
         procedure, nopass  :: bind_b => proc3
      end type
  
      contains
      integer function proc1()
         proc1 = 100
      end function 
     
      integer function proc2()
         proc2 = 200
      end function 

      subroutine proc3()
      end subroutine 

   end module     
   
   end
   
