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
! %POSTCMD: dcomp dtybn014a.f 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtybn014a.f
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : binding attributes 
!*
!*  SECONDARY FUNCTIONS TESTED : nopass 
!*
!*  DESCRIPTION                : if =>procedure-name appears, the 
!*                               double-colon separator shall 
!*                               appear.
!*                            
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1	      
      type parent
         integer :: x
	 contains
      	 procedure, nopass  bind => proc 
      end type

      type(parent) :: dt_p

      contains
      subroutine proc(arg1)
         class(parent) :: arg1
      end subroutine

   end module     

!   use mod1

   end
   
