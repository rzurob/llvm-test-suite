!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn091u.f 
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
!*  TEST CASE NAME             : ftybn091u.f 
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DESCRIPTION                : testing nopass binding has no effects
!*                               on state of the calling object 
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod	      
      integer :: int = 200
      character*20 :: c = "hi"

      type parent 
         integer :: x
	 contains
      	 procedure, nopass :: bind => proc1
      end type 

      type, extends(parent) :: child 
      contains
         procedure, nopass :: bind => proc1
      end type  

      type, extends(child) :: thirGen 
      contains
         procedure, nopass :: bind => proc2
      end type

      type(parent) :: dt_p = parent(10)
      type(child) :: dt_c = child(20)
      type(thirGen) :: dt_g3 = thirGen(30)
 
      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2()
         int = 0
         c = "hi"
      end subroutine

   end module
   
   use mod

   if (int .ne. 200)       error stop 2
   if (c .ne. "hi")       error stop 3

   call dt_p%bind()
   if (int .ne. 400)       error stop 4
   if (c .ne. "hi_again") error stop 5
   if (dt_p%x .ne. 10)     error stop 55 

   call proc2()
   call dt_c%bind()
   if (int .ne. 400)       error stop 6
   if (c .ne. "hi_again") error stop 7
   if (dt_c%x .ne. 20)     error stop 77

   call dt_g3%bind()
   if (int .ne. 0)      error stop 8
   if (c .ne. "hi")    error stop 9
   if (dt_g3%x .ne. 30) error stop 99

   end

   
