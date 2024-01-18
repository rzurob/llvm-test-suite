!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn091r.f 
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
!*  TEST CASE NAME             : ftybn091r.f 
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
!*  DESCRIPTION                : Testing inheritance with non_overridable 
!*                               attribute.e.
!* 
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod	      
      integer :: int = 200
      character*20 :: c = "hi"

      type base1 
         integer :: x
	 contains
      	 procedure, nopass, non_overridable :: bind => proc1
      end type 

      type base2 
         type(base1) :: x 
      contains
         procedure, nopass, non_overridable ::  proc1 
      end type  

      type base3 
         type(base2) :: x
      contains
         procedure, nopass, non_overridable :: bind => proc2
      end type  

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine
     
      subroutine proc2()
         int = 200
         c = "hi"
      end subroutine

   end module     

   use mod

   type(base1) :: dt1
   type(base2) :: dt2
   type(base3) :: dt3

   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
  
   call dt1%bind()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5

   int = 0
   c = ""
   call dt2%proc1()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
 
   int = 0
   c = ""
   call dt2%x%bind()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9

   int = 0
   c = ""
   call dt3%bind()
   if (int .ne. 200)      error stop 10
   if (c .ne. "hi")    error stop 11

   call dt3%x%x%bind()
   if (int .ne. 400)      error stop 11
   if (c .ne. "hi_again")    error stop 12

   end
   
