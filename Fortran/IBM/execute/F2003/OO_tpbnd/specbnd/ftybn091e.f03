!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : parent procedures are inherited.
!*                               inherite from a different scroping unit.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

	module mod1
      integer :: int = 200
      character*20 :: c = "hi"

		type base
         integer :: x
		contains
      	procedure, nopass :: bind_b => proc1
		end type base

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine
	end module

   module mod2
   use mod1
   type, extends(base) :: parent
      integer :: y
   end type
   end module mod2

   use mod2

   type(base) :: dt
   type(parent) :: dt_p
   if (int .ne. 200)      error stop 2
   if (c .ne. "hi")    error stop 3
   call dt%bind_b()
   if (int .ne. 400)      error stop 4
   if (c .ne. "hi_again")    error stop 5
   int = 0
   c = ""
   call dt_p%bind_b()
   if (int .ne. 400)      error stop 6
   if (c .ne. "hi_again")    error stop 7

   end
