!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn096h.f 
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
!*  TEST CASE NAME             : ftybn092h.f 
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
!*                               with multiple levels inheritance. 
!*                               inherite from a different scoping units.
!* 
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod	      
      integer :: int = 200
      character*20 :: c = "hi"

      type base 
         integer :: x
	 contains
      	 procedure, pass :: bind_b => proc1
         procedure, pass :: bind_r => proc2
      end type 

      contains
      subroutine proc1(arg1)
         class(base) :: arg1
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2(arg1)
         class(base) :: arg1
         int = 0
         c = ""
      end subroutine

   end module     
   
   module mod1
      use mod
      type, extends(base) :: parent
      end type
   end module

   module mod2
      use mod1
      type, extends(parent) :: child
      end type
   end module

   module mod3
      use mod2
      type, extends(child) :: thirGen
      end type
   end module

   module mod4
      use mod3
      type, extends(thirGen) :: fourGen
      end type
   end module

   module mod5
      use mod4
      type, extends(fourGen) :: fifGen
      end type
   end module

   use mod5

   type(base) :: dt
   type(parent) :: dt_p
   type(child) :: dt_c
   type(thirGen) :: dt_3
   type(fourGen) :: dt_4
   type(fifGen) :: dt_5
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt%bind_b()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   call dt_p%bind_r()
   call dt_p%bind_b()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
   call dt_c%bind_r()
   call dt_c%bind_b()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9
   call dt_3%bind_r()
   call dt_3%bind_b()
   if (int .ne. 400)      error stop 12
   if (c .ne. "hi_again")    error stop 13

   call dt_4%bind_r()
   call dt_4%bind_b()
   if (int .ne. 400)      error stop 14
   if (c .ne. "hi_again")    error stop 15

   call dt_5%bind_r()
   call dt_5%bind_b()
   if (int .ne. 400)      error stop 16
   if (c .ne. "hi_again")    error stop 17

   end
   
