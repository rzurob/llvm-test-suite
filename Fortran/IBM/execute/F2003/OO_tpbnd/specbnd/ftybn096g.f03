!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : pass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance
!*
!*  DESCRIPTION                : parent procedures are inherited.
!*                               with two levels inheritance.
!*                               inherite from a different scoping units.
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
   type, extends(parent) :: child
   end type
   end module

   module mod2
   use mod1
   type, extends(child) :: thirGen
   end type
   end module

   use mod2

   type(parent) :: dt_p
   type(child) :: dt_c
   type(thirGen) :: dt_3g
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt_p%bind()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   call dt_c%bind_r()
   call dt_c%bind()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7
   call dt_3g%bind_r()
   call dt_3g%bind()
   if (int .ne. 400)      error stop 8
   if (c .ne. "hi_again")    error stop 9
   end
