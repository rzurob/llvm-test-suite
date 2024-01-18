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
! %POSTCMD: dcomp ftybn092f.f ftybn092f.vf 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn092f.f 
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
!*  DESCRIPTION                : testing accessiblity overriding with two
!*                               types which all extend from the base
!*                               type, but overriding the type-bound
!*                               procedures of the base type differently.
!*                               
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
         integer :: y
      contains
         procedure, nopass :: bind_b => proc1
      end type  

      type, extends(base) :: parent2
         integer :: z
      contains
!* expected the error message 1514-588 (S) here
         procedure, nopass,private  :: bind_b => proc1
      end type
  
      type(base) :: dt

      contains
      subroutine proc1()
      end subroutine
      end module     

   end
   
