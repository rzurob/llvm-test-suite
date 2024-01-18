! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn096i.f
! opt variations: -ql

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn096i.f 
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
!*  TEST CASE NAME             : ftybn096i.f 
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
!*  DESCRIPTION                : testing a procedure is bound to different 
!*                               types. 
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod	      
      integer :: int = 200
      character*20 :: c = "hi"

      type base1(k1)    ! (4) 
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure, pass(arg1) :: bind_b1 => proc1
      end type 

      type base2(k2)    ! (4) 
         integer, kind :: k2
         integer(k2)   :: x
      contains
         procedure, pass(arg2) :: bind_b2 => proc1
      end type  

      type base3(k3)    ! (4) 
         integer, kind :: k3
         integer(k3)   :: x
      contains
         procedure, pass(arg3) :: bind_b3 => proc1
      end type  

      contains
      subroutine proc1(arg1, arg2, arg3)
         class(base1(4)) :: arg1
         class(base2(4)) :: arg2
         class(base3(4)) :: arg3
         int = 400
         c = "hi_again"
      end subroutine

	end module     

   use mod

   type(base1(4)) :: dt1
   type(base2(4)) :: dt2
   type(base3(4)) :: dt3
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt1%bind_b1(dt2, dt3)
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   int = 0
   c = ""
   call dt2%bind_b2(dt1, dt3)
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
   int = 0
   c = ""
   call dt3%bind_b3(dt1, dt2)
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   end
   
