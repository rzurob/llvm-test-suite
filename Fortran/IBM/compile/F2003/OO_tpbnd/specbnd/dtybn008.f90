!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS: 
! %POSTCMD: dcomp dtybn008.f 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtybn007.f 
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : private subroutine 
!*
!*  SECONDARY FUNCTIONS TESTED : nopass , non_overridable
!*
!*  DESCRIPTION                : testing the private type bound
!*                               procedure within a private derived
!*                               type. 
!*                               
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1	      
      type,private :: parent
         integer :: x
	 contains
      	 procedure, private, nopass, non_overridable :: bind => proc1
      end type 

   type(parent) :: dt_p

      contains
      subroutine proc1()
      end subroutine
   
   end module     

   use mod1
   call dt_p%bind()

   end
   
