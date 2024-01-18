!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn096f.f 
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
!*  TEST CASE NAME             : ftybn096f.f 
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : pass binding attribute 
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance 
!*
!*  DESCRIPTION                : parent procedures are inherited.
!*                               with more than one type extend base 
!*                               type from different scoping units.
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod	      
      integer :: int = 200
      character*20 :: c = "hi"

      type parent
         integer :: x
	 contains
      	 procedure, pass :: bind => proc1
         procedure, pass :: bind_r => proc2
	 end type 

      contains
      subroutine proc1(arg1)
         class(parent) :: arg1
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2(arg1)
         class(parent) :: arg1
         int = 0
         c = ""
      end subroutine
	end module     
   
   module mod1
   use mod
   type, extends(parent) :: child1
   end type
   end module

   module mod2
   use mod
   type, extends(parent) :: child2
   end type
   end module

   use mod1
   use mod2

   type(parent) :: dt_p
   type(child1) :: dt_c1
   type(child2) :: dt_c2
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt_p%bind()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   call dt_c1%bind_r()
   call dt_c1%bind()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
   call dt_c2%bind_r()
   call dt_c2%bind()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9
   end
   
