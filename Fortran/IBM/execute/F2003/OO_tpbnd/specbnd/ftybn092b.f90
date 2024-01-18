!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn092b.f 
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn092b.f 
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
  
      integer :: int = 200
      character*20 :: char = "hi"

      type base 
         integer :: x
	 contains
      	 procedure, nopass, private :: bind_b => proc1
      end type 

      type, extends(base) :: parent1
      contains
         procedure, nopass, public :: bind_b => proc1
      end type  

!*   if no private attribute specifies, the default is public
!*   accessiblity
  
      type, extends(base) :: parent2
      contains
         procedure, nopass  :: bind_b => proc1
      end type
  
      type(base) :: dt

      contains
      subroutine proc1()
         int = 400
         char = "hi_again"
      end subroutine
     
      subroutine proc2(arg1)
         type(base) :: arg1 
         call arg1%bind_b()
      end subroutine

   end module     

   use mod
   
   type(base)    :: dt
   type(parent1) :: dt_p1
   type(parent2) :: dt_p2
   if (int .ne. 200)      error stop 2
   if (char .ne. "hi")    error stop 3

   call proc2(dt)
   if (int .ne. 400)      error stop 4
   if (char .ne. "hi_again")    error stop 5
  
   int = 0
   char = ""
   call dt_p1%bind_b()
   if (int .ne. 400)      error stop 6
   if (char .ne. "hi_again")    error stop 7

   int = 0
   char = ""
   call dt_p2%bind_b()
   if (int .ne. 400)      error stop 8
   if (char .ne. "hi_again")    error stop 9
  
   end
   
