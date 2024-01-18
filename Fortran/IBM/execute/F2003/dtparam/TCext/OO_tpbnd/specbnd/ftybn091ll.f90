! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn091ll.f
! opt variations: -ql

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn091ll.f 
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
!*  TEST CASE NAME             : ftybn091ll.f 
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
!*  DESCRIPTION                : testing the base procedure is bound to
!*                               multiple level inherited types with 
!*                               different binding-names within different
!*                               scopes.
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1	      
      integer :: int = 200
      character*20 :: c = "hi"

      type parent(k1)    ! (4) 
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure, nopass :: bind_b => proc1
         procedure, nopass :: bind_r => proc2
		end type parent 
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
      type, extends(parent) :: child    ! (4)
      contains
         procedure, nopass :: bind_c => proc1
      end type
   end module

   module mod3
   use mod2
      type, extends(child) :: thirGen    ! (4)
      contains
         procedure, nopass :: bind_3 => proc1
      end type
   end module

   module mod4
   use mod3
      type, extends(thirGen) :: fourGen    ! (4)
      contains
         procedure, nopass :: bind_4 => proc1
      end type
   end module

   module mod5
   use mod4
      type, extends(fourGen) :: fifGen    ! (4)
      contains
         procedure, nopass :: bind_5 => proc1
      end type
   end module

   use mod5

   type(parent(4)) :: dt_p
   type(child(4)) :: dt_c
   type(thirGen(4)) :: dt_3
   type(fourGen(4)) :: dt_4
   type(fifGen(4)) :: dt_5
  
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
  
   call dt_p%bind_r()
   call dt_p%bind_b()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
 
   call dt_c%bind_r()
   call dt_c%bind_c()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9

   call dt_3%bind_r()
   call dt_3%bind_3()
   if (int .ne. 400)      error stop 12
   if (c .ne. "hi_again")    error stop 13

   call dt_4%bind_r()
   call dt_4%bind_4()
   if (int .ne. 400)      error stop 14
   if (c .ne. "hi_again")    error stop 15

   call dt_5%bind_r()
   call dt_5%bind_5()
   if (int .ne. 400)      error stop 16
   if (c .ne. "hi_again")    error stop 17

   end
   
