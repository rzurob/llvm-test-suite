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
! %POSTCMD: dcomp dtybn010.f 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtybn010.f 
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : procedure name 
!*
!*  SECONDARY FUNCTIONS TESTED : nopass, non_overridable 
!*
!*  DESCRIPTION                : if neither =>procedure-name nor interface-name
!*                               appears, it is as though =>procedure-name    
!*                               had appeared with a procedure name the same
!*                               as the binding name.
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1	      
      type parent
         integer :: x
	 contains
      	 procedure, private, nopass, non_overridable :: bind 
      end type 

      type(parent) :: dt_p

      contains
      subroutine bind(arg1)
         class(parent) :: arg1
      end subroutine

   subroutine test
      call dt_p%bind(dt_p)
   end subroutine

   end module     

   use mod1

   call test 

   end
   
