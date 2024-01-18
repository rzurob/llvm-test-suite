! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_tpbnd/specbnd/ftybn091kk.f
! opt variations: -qnok -qnol

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn091kk.f 
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
!*  TEST CASE NAME             : ftybn091kk.f 
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
!*  DESCRIPTION                : testing the base procedure is bounded to 
!*                               two types which all extend the base type,
!*                               with different binding-names within 
!*                               different scopes. 
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1	      
      integer :: int = 200
      character*20 :: c = "hi"

      type base(n1,k1)    ! (20,4) 
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
      contains
      	 procedure, nopass :: bind_b => proc1
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
   
   module mod2
   use mod1
      type, extends(base) :: parent1(k2,n2)    ! (20,4,4,20)
          integer, kind :: k2
          integer, len  :: n2
      contains
         procedure, nopass :: bind_p1 => proc1
      end type
   end module

   module mod3
   use mod1
      type, extends(base) :: parent2(k3,n3)    ! (20,4,4,20)
          integer, kind :: k3
          integer, len  :: n3
      contains
         procedure, nopass :: bind_p2 => proc1
      end type
   end module

   use mod2
   use mod3

   type(base(20,4)) :: dt
   type(parent1(20,4,4,20)) :: dt_p1
   type(parent2(20,4,4,20)) :: dt_p2
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt%bind_b()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5

   call proc2()
   call dt_p1%bind_p1()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
  
   call proc2()
   call dt_p2%bind_p2()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9
   end
   
