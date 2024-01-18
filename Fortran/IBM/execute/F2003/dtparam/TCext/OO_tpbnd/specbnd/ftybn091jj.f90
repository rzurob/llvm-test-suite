! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn091jj.f
! opt variations: -ql

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn091jj.f 
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
!*  TEST CASE NAME             : ftybn091jj.f 
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
!*  DESCRIPTION                : testing a procedure is bound to both
!*                                parent and child types,
!*                                but with different binding-names within 
!*                                different scopes. 
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod1	      
      integer :: int = 200
      character*20 :: c = "hi"

      type base(k1)    ! (4) 
         integer, kind :: k1
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
      type, extends(base) :: parent    ! (4)
      contains
         procedure, nopass :: bind_p => proc1
      end type
   end module 

   module mod3
      use mod2
      type, extends(parent) :: child    ! (4)
      contains
         procedure, nopass :: bind_c => proc1
      end type
   end module 

   use mod3

   type(base(4)) :: dt
   type(parent(4)) :: dt_p
   type(child(4)) :: dt_c
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3

   call dt%bind_b()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5

   call proc2()
   call dt_p%bind_p()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   call proc2()
   call dt_c%bind_c()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9
   end
   
