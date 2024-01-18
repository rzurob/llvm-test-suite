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
! %POSTCMD: dcomp ftybn091t.f 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn091t.f 
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute 
!*
!*  SECONDARY FUNCTIONS TESTED : non_overridable 
!*
!*  DESCRIPTION                : testing the parent procedures are    
!*                               overridden, with multiple levels     
!*                               overridden. 
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod	      
      integer :: int = 200
      character*20 :: c = "hi"

      type parent 
         integer :: x
	 contains
      	 procedure, nopass, non_overridable :: bind => proc1
         procedure, nopass, non_overridable :: bind_r => proc2
      end type 

      type, extends(parent) :: child 
         type(parent) :: child
      contains
!* expect error message here: overridding isn't allowed with the non_overridable!* attribute in the parent type.
         procedure, nopass :: bind => proc1 
      end type  

!* expect error message here: Component name child of derived type definition thirgen, exists in parent type child.  A component in an extended type can not have the same name as any accessible component of its parent type.
 
      type, extends(child) :: thirGen
         type(child) :: y 
      contains
         procedure, nopass :: bind => proc1
      end type

      type, extends(thirGen) :: fourGen
      contains
         procedure, nopass :: bind => proc1
      end type

      type, extends(fourGen) :: fifGen 
      contains
         procedure, nopass :: bind => proc1
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2()
         int = 0
         c = ""
      end subroutine

   end module     

   end
   
